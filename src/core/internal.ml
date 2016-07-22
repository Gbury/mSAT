(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module Make
    (St : Solver_types.S)
    (Plugin : Plugin_intf.S with type term = St.term
                             and type formula = St.formula
                             and type proof = St.proof)
    (Dummy: sig end)
= struct

  module Proof = Res.Make(St)

  open St

  exception Sat
  exception Unsat
  exception UndecidedLit
  exception Restart
  exception Conflict of clause

  (* a push/pop state *)
  type user_level = {
    (* User levels always refer to decision_level 0 *)
    ul_elt_lvl : int;     (* Number of atoms in trail at decision level 0 *)
    ul_th_lvl : int;      (* Number of atoms known by the theory at decicion level 0 *)
    ul_th_env : Plugin.level; (* Theory state at level 0 *)
    ul_clauses : int;     (* number of clauses *)
    ul_learnt : int;      (* number of learnt clauses *)
  }

  (* Singleton type containing the current state *)
  type env = {

    clauses_hyps : clause Vec.t;
    (* clauses assumed (subject to user levels) *)
    clauses_learnt : clause Vec.t;
    (* learnt clauses (true at anytime, whatever the user level) *)
    clauses_pushed : clause Stack.t;
    (* Clauses pushed, waiting to be added as learnt. *)


    mutable unsat_conflict : clause option;
    (* conflict clause at decision level 0, if any *)
    mutable next_decision : atom option;
    (* When the last conflict was a semantic one, this stores the next decision to make *)

    elt_queue : t Vec.t;
    (* decision stack + propagated elements (atoms or assignments) *)

    elt_levels : int Vec.t;
    (* decision levels in [trail]  *)
    th_levels : Plugin.level Vec.t;
    (* theory states corresponding to elt_levels *)
    user_levels : user_level Vec.t;
    (* user-defined levels, for {!push} and {!pop} *)

    mutable th_head : int;
    (* Start offset in the queue of unit fact not yet seen by the theory *)
    mutable elt_head : int;
    (* Start offset in the queue of unit facts to propagate, within the trail *)


    mutable simpDB_props : int;
    (* remaining number of propagations before the next call to [simplify ()] *)
    mutable simpDB_assigns : int;
    (* number of toplevel assignments since last call to [simplify ()] *)


    order : Iheap.t;
    (* Heap ordered by variable activity *)

    var_decay : float;
    (* inverse of the activity factor for variables. Default 1/0.999 *)
    clause_decay : float;
    (* inverse of the activity factor for clauses. Default 1/0.95 *)

    mutable var_incr : float;
    (* increment for variables' activity *)
    mutable clause_incr : float;
    (* increment for clauses' activity *)


    mutable progress_estimate : float;
    (* progression estimate, updated by [search ()] *)

    remove_satisfied : bool;
    (* Wether to remove satisfied learnt clauses when simplifying *)


    restart_inc : float;
    (* multiplicative factor for restart limit, default 1.5 *)
    mutable restart_first : int;
    (* intial restart limit, default 100 *)


    learntsize_inc : float;
    (* multiplicative factor for [learntsize_factor] at each restart, default 1.1 *)
    mutable learntsize_factor : float;
    (* initial limit for the number of learnt clauses, 1/3 of initial
        number of clauses by default *)

    mutable starts : int;
    mutable decisions : int;
    mutable propagations : int;
    mutable conflicts : int;
    mutable clauses_literals : int;
    mutable learnts_literals : int;
    mutable nb_init_clauses : int;
  }

  (* Starting environment. *)
  let env = {
    unsat_conflict = None;
    next_decision = None;

    clauses_hyps = Vec.make 0 dummy_clause;
    clauses_learnt = Vec.make 0 dummy_clause;
    clauses_pushed = Stack.create ();

    th_head = 0;
    elt_head = 0;

    elt_queue = Vec.make 601 (of_atom dummy_atom);
    elt_levels = Vec.make 601 (-1);
    th_levels = Vec.make 100 Plugin.dummy;

    user_levels = Vec.make 20 {
        ul_elt_lvl = 0;
        ul_th_lvl = 0;
        ul_learnt = 0;
        ul_clauses = 0;
        ul_th_env = Plugin.dummy;
      };

    order = Iheap.init 0;

    var_incr = 1.;
    clause_incr = 1.;
    var_decay = 1. /. 0.95;
    clause_decay = 1. /. 0.999;

    simpDB_assigns = -1;
    simpDB_props = 0;

    progress_estimate = 0.;
    remove_satisfied = false;

    restart_inc = 1.5;
    restart_first = 100;

    learntsize_factor = 1. /. 3. ;
    learntsize_inc = 1.1;

    starts = 0;
    decisions = 0;
    propagations = 0;
    conflicts = 0;
    clauses_literals = 0;
    learnts_literals = 0;
    nb_init_clauses = 0;
  }

  (* Misc functions *)
  let to_float i = float_of_int i
  let to_int f = int_of_float f

  let nb_clauses () = Vec.size env.clauses_hyps
  let nb_vars    () = St.nb_elt ()
  let decision_level () = Vec.size env.elt_levels

  let f_weight i j =
    get_elt_weight (St.get_elt j) < get_elt_weight (St.get_elt i)

  (* Is the assumptions currently unsat ? *)
  let is_unsat () =
    match env.unsat_conflict with
    | Some _ -> true
    | None -> false

  (* Level for push/pop operations *)
  type level = int

  (* Push/Pop *)
  let current_level () = Vec.size env.user_levels

  let push () : level =
    if is_unsat () then
      (* When unsat, pushing does nothing, since adding more assumptions
         can not make the proof disappear. *)
      current_level ()
    else begin
      (* The assumptions are sat, or at least not yet detected unsat,
         we need to save enough to be able to restore the current decision
         level 0. *)
      let res = current_level () in
      (* To restore decision level 0, we need the stolver queue, and theory state. *)
      let ul_elt_lvl, ul_th_lvl =
        if Vec.is_empty env.elt_levels then
          env.elt_head, env.th_head
        else
          let l = Vec.get env.elt_levels 0 in
          l, l
      and ul_th_env =
        if Vec.is_empty env.th_levels then Plugin.current_level ()
        else Vec.get env.th_levels 0
      in
      (* Keep in mind what are the current assumptions. *)
      let ul_clauses = Vec.size env.clauses_hyps in
      let ul_learnt = Vec.size env.clauses_learnt in
      Vec.push env.user_levels {ul_elt_lvl; ul_th_lvl; ul_th_env; ul_clauses; ul_learnt;};
      res
    end

  (* To store info for level 0, it is easier to push at module
     initialisation, when there are no assumptions. *)
  let base_level =
    let l = push () in
    assert (l = 0);
    l

  (* Iteration over subterms.
     When incrementing activity, we want to be able to iterate over
     all subterms of a formula. However, the function provided by the theory
     may be costly (if it walks a tree-like structure, and does some processing
     to ignore some subterms for instance), so we want to 'cache' to list
     of subterms of each formula. To do so we use a hashtable from variable id to
     list of subterms. *)
  let iter_map = Hashtbl.create 1003

  let iter_sub f v =
    try
      List.iter f (Hashtbl.find iter_map v.vid)
    with Not_found ->
      let l = ref [] in
      Plugin.iter_assignable (fun t -> l := add_term t :: !l) v.pa.lit;
      Hashtbl.add iter_map v.vid !l;
      List.iter f !l

  (* When we have a new litteral,
     we need to first create the list of its subterms. *)
  let atom lit : atom =
    let res = add_atom lit in
    iter_sub ignore res.var;
    res

  (* Variable and litteral activity.
     Activity is used to decide on which variable to decide when propagation
     is done. Uses a heap (implemented in Iheap), to keep track of variable activity.
     To be more general, the heap only stores the variable/litteral id (i.e an int).
     When we add a variable (which wraps a formula), we also need to add all
     its subterms.
  *)
  let insert_var_order = function
    | E_lit l -> Iheap.insert f_weight env.order l.lid
    | E_var v ->
      Iheap.insert f_weight env.order v.vid;
      iter_sub (fun t -> Iheap.insert f_weight env.order t.lid) v

  (* Rather than iterate over all the heap when we want to decrease all the
     variables/literals activity, we instead increase the value by which
     we increase the activity of 'interesting' var/lits. *)
  let var_decay_activity () =
    env.var_incr <- env.var_incr *. env.var_decay

  let clause_decay_activity () =
    env.clause_incr <- env.clause_incr *. env.clause_decay

  let var_bump_activity_aux v =
    v.v_weight <- v.v_weight +. env.var_incr;
    if v.v_weight > 1e100 then begin
      for i = 0 to (St.nb_elt ()) - 1 do
        set_elt_weight (St.get_elt i) ((get_elt_weight (St.get_elt i)) *. 1e-100)
      done;
      env.var_incr <- env.var_incr *. 1e-100;
    end;
    if Iheap.in_heap env.order v.vid then
      Iheap.decrease f_weight env.order v.vid

  let lit_bump_activity_aux l =
    l.l_weight <- l.l_weight +. env.var_incr;
    if l.l_weight > 1e100 then begin
      for i = 0 to (St.nb_elt ()) - 1 do
        set_elt_weight (St.get_elt i) ((get_elt_weight (St.get_elt i)) *. 1e-100)
      done;
      env.var_incr <- env.var_incr *. 1e-100;
    end;
    if Iheap.in_heap env.order l.lid then
      Iheap.decrease f_weight env.order l.lid

  let var_bump_activity v =
    var_bump_activity_aux v;
    iter_sub lit_bump_activity_aux v

  let clause_bump_activity c =
    c.activity <- c.activity +. env.clause_incr;
    if c.activity > 1e20 then begin
      for i = 0 to (Vec.size env.clauses_learnt) - 1 do
        (Vec.get env.clauses_learnt i).activity <-
          (Vec.get env.clauses_learnt i).activity *. 1e-20;
      done;
      env.clause_incr <- env.clause_incr *. 1e-20
    end

  (* Simplification of clauses.
     When adding new clauses, it is desirable to 'simplify' them, i.e:
     - remove variables that are false at level 0, since it is a fact
       that they cannot be true, and therefore can not help to satisfy the clause

     Aditionally, since we can do push/pop on the assumptions, we need to
     keep track of what assumptions were used to simplify a given clause.
  *)
  exception Trivial

  let simplify_zero atoms =
    (* Eliminates dead litterals from clauses when at decision level 0 (see above) *)
    assert (decision_level () = 0);
    let aux (atoms, history) a =
      if a.is_true then raise Trivial;
      (* If a variable is true at level 0, then the clause is always satisfied *)
      if a.neg.is_true then begin
        (* If a variable is false, we need to see why it is false. *)
        match a.var.reason with
        | None | Some Decision -> assert false
        (* The var must have a reason, and it cannot be a decision, since we are
           at level 0. *)
        | Some (Bcp cl) -> atoms, cl :: history
        (* The variable has been set to false because of another clause,
           we then need to keep track of the assumption level used. *)
        | Some (Semantic 0) -> atoms, history
        (* Semantic propagations at level 0 are, well not easy to deal with,
           this shouldn't really happen actually (because semantic propagations
           at level 0 currently lack a proof). *)
        | Some (Semantic _) ->
          Log.debugf 0 "Unexpected semantic propagation at level 0: %a"
            (fun k->k St.pp_atom a);
          assert false
      end else
        a::atoms, history
        (* General case, we do not know the truth value of a, just let it be. *)
    in
    let atoms, init = Array.fold_left aux ([], []) atoms in
    (* TODO: Why do we sort the atoms here ? *)
    List.fast_sort (fun a b -> a.var.vid - b.var.vid) atoms, init

  let arr_to_list arr i =
    if i >= Array.length arr then []
    else Array.to_list (Array.sub arr i (Array.length arr - i))

  let partition atoms =
    (* Parittion litterals for new clauses *)
    let rec partition_aux trues unassigned falses history i =
      if i >= Array.length atoms then
        trues @ unassigned @ falses, history
      else begin
        let a = atoms.(i) in
        if a.is_true then
          if a.var.v_level = 0 then
            raise Trivial
            (* A var true at level 0 gives a trivially true clause *)
          else
            (a :: trues) @ unassigned @ falses @
            (arr_to_list atoms (i + 1)), history
          (* A var true at level > 0 does not change anything, but is unlikely
             to be watched, so we put prefer to put them at the end. *)
        else if a.neg.is_true then
          if a.var.v_level = 0 then begin
            match a.var.reason with
            | Some (Bcp cl) ->
              partition_aux trues unassigned falses (cl :: history) (i + 1)
            (* Same as before, a var false at level 0 can be eliminated from the clause,
               but we need to kepp in mind that we used another clause to simplify it. *)
            | Some (Semantic 0) ->
              partition_aux trues unassigned falses history (i + 1)
            | _ -> assert false
          end else
            partition_aux trues unassigned (a::falses) history (i + 1)
        else
          partition_aux trues (a::unassigned) falses history (i + 1)
      end
    in
    if decision_level () = 0 then
      simplify_zero atoms
    else
      partition_aux [] [] [] [] 0

  (* Compute a progess estimate.
     TODO: remove it or use it ? *)
  let progress_estimate () =
    let prg = ref 0. in
    let nbv = to_float (nb_vars()) in
    let lvl = decision_level () in
    let _F = 1. /. nbv in
    for i = 0 to lvl do
      let _beg = if i = 0 then 0 else Vec.get env.elt_levels (i-1) in
      let _end = if i=lvl then Vec.size env.elt_queue else Vec.get env.elt_levels i in
      prg := !prg +. _F**(to_float i) *. (to_float (_end - _beg))
    done;
    !prg /. nbv


  (* Making a decision.
     Before actually creatig a new decision level, we check that
     all propagations have been done and propagated to the theory,
     i.e that the theoriy state indeed takes into account the whole
     stack of litterals
     i.e we have indeed reached a propagation fixpoint before making
     a new decision *)
  let new_decision_level() =
    assert (env.th_head = Vec.size env.elt_queue);
    assert (env.elt_head = Vec.size env.elt_queue);
    Vec.push env.elt_levels (Vec.size env.elt_queue);
    Vec.push env.th_levels (Plugin.current_level ()); (* save the current tenv *)
    ()

  (* Attach/Detach a clause.
     Clauses that become satisfied are detached, i.e we remove
     their watchers, while clauses that loose their satisfied status
     have to be reattached, adding watchers. *)
  let attach_clause c =
    if not c.attached then begin
      Log.debugf 60 "Attaching %a" (fun k -> k St.pp_clause c);
      c.attached <- true;
      Vec.push c.atoms.(0).neg.watched c;
      Vec.push c.atoms.(1).neg.watched c;
    end

  let detach_clause c =
    if c.attached then begin
      c.attached <- false;
      Log.debugf 10 "Removing clause @[%a@]" (fun k->k St.pp_clause c);
      Vec.remove c.atoms.(0).neg.watched c;
      Vec.remove c.atoms.(1).neg.watched c;
    end

  (* Is a clause satisfied ? *)
  let satisfied c = Array_util.exists (fun atom -> atom.is_true) c.atoms

  (* Backtracking.
     Used to backtrack, i.e cancel down to [lvl] excluded,
     i.e we want to go back to the state the solver was in
         when decision level [lvl] was created. *)
  let cancel_until lvl =
    (* Nothing to do if we try to backtrack to a non-existent level. *)
    if decision_level () > lvl then begin
      Log.debugf 5 "Backtracking to lvl %d" (fun k -> k lvl);
      (* We set the head of the solver and theory queue to what it was. *)
      env.elt_head <- Vec.get env.elt_levels lvl;
      env.th_head <- env.elt_head;
      (* Now we need to cleanup the vars that are not valid anymore
         (i.e to the right of elt_head in the queue. *)
      for c = env.elt_head to Vec.size env.elt_queue - 1 do
        match (Vec.get env.elt_queue c) with
        (* A literal is unassigned, we nedd to add it back to
           the heap of potentially assignable literals. *)
        | Lit l ->
          l.assigned <- None;
          l.l_level <- -1;
          insert_var_order (elt_of_lit l)
        (* A variable is not true/false anymore, one of two things can happen: *)
        | Atom a ->
          if a.var.v_level <= lvl then begin
            (* It is a semantic propagation, which can be late, and has a level
               lower than where we backtrack, so we just move it to the head
               of the queue. *)
            Vec.set env.elt_queue env.elt_head (of_atom a);
            env.elt_head <- env.elt_head + 1
          end else begin
            (* it is a result of bolean propagation, or a semantic propagation
               with a level higher than the level to which we backtrack,
               in that case, we simply unset its value and reinsert it into the heap. *)
            a.is_true <- false;
            a.neg.is_true <- false;
            a.var.v_level <- -1;
            a.var.reason <- None;
            insert_var_order (elt_of_var a.var)
          end
      done;
      (* Recover the right theory state. *)
      Plugin.backtrack (Vec.get env.th_levels lvl);
      (* Resize the vectors according to their new size. *)
      Vec.shrink env.elt_queue ((Vec.size env.elt_queue) - env.elt_head);
      Vec.shrink env.elt_levels ((Vec.size env.elt_levels) - lvl);
      Vec.shrink env.th_levels ((Vec.size env.th_levels) - lvl);
    end;
    assert (Vec.size env.elt_levels = Vec.size env.th_levels);
    ()

  (* Unsatisfiability is signaled through an exception, since it can happen
     in multiple places (adding new clauses, or solving for instance). *)
  let report_unsat ({atoms=atoms} as confl) =
    Log.debugf 5 "@[Unsat conflict: %a@]" (fun k -> k St.pp_clause confl);
    env.unsat_conflict <- Some confl;
    raise Unsat

  (* Simplification of boolean propagation reasons.
     When doing boolean propagation *at level 0*, it can happen
     that the clause cl, which propagates a formula, also contains
     other formulas, but has been simplified. in which case, we
     need to rebuild a clause with correct history, in order to
     be able to build a correct proof at the end of proof search. *)
  let simpl_reason = function
    | (Bcp cl) as r ->
      let l, history = partition cl.atoms in
      begin match l with
        | [ a ] ->
          if history = [] then r
          (* no simplification has been done, so [cl] is actually a clause with only
             [a], so it is a valid reason for propagating [a]. *)
          else
            (* Clauses in [history] have been used to simplify [cl] into a clause [tmp_cl]
               with only one formula (which is [a]). So we explicitly create that clause
               and set it as the cause for the propagation of [a], that way we can
               rebuild the whole resolution tree when we want to prove [a]. *)
            Bcp (make_clause (fresh_tname ()) l (History (cl :: history)))
        | _ -> assert false
      end
    | r -> r

  (* Boolean propagation.
     Wrapper function for adding a new propagated formula. *)
  let enqueue_bool a lvl reason =
    if a.neg.is_true then begin
      Log.debugf 0 "Trying to enqueue a false litteral: %a" (fun k->k St.pp_atom a);
      assert false
    end;
    if not a.is_true then begin
      assert (a.var.v_level < 0 && a.var.reason = None && lvl >= 0);
      let reason =
        if lvl > 0 then reason
        else simpl_reason reason
      in
      a.is_true <- true;
      a.var.v_level <- lvl;
      a.var.reason <- Some reason;
      Vec.push env.elt_queue (of_atom a);
      Log.debugf 20 "Enqueue (%d): %a"
        (fun k->k (Vec.size env.elt_queue) pp_atom a)
    end

  let enqueue_assign l value lvl =
    l.assigned <- Some value;
    l.l_level <- lvl;
    Vec.push env.elt_queue (of_lit l);
    ()

  let th_eval a =
    if a.is_true || a.neg.is_true then None
    else match Plugin.eval a.lit with
      | Plugin_intf.Unknown -> None
      | Plugin_intf.Valued (b, lvl) ->
        let atom = if b then a else a.neg in
        enqueue_bool atom lvl (Semantic lvl);
        Some b

  (* conflict analysis *)
  let max_lvl_atoms l =
    List.fold_left (fun (max_lvl, acc) a ->
        if a.var.v_level = max_lvl then (max_lvl, a :: acc)
        else if a.var.v_level > max_lvl then (a.var.v_level, [a])
        else (max_lvl, acc)) (0, []) l

  let backtrack_lvl is_uip = function
    | [] -> 0
    | a :: r when not is_uip -> max (a.var.v_level - 1) 0
    | a :: [] -> 0
    | a :: b :: r ->
      assert(a.var.v_level <> b.var.v_level);
      b.var.v_level

  let analyze_mcsat c_clause =
    let tr_ind  = ref (Vec.size env.elt_queue) in
    let is_uip  = ref false in
    let c       = ref (Proof.to_list c_clause) in
    let history = ref [c_clause] in
    clause_bump_activity c_clause;
    let is_semantic a = match a.var.reason with
      | Some Semantic _ -> true
      | _ -> false
    in
    try while true do
        let lvl, atoms = max_lvl_atoms !c in
        if lvl = 0 then raise Exit;
        match atoms with
        | [] | _ :: [] ->
          is_uip := true;
          raise Exit
        | _ when List.for_all is_semantic atoms ->
          raise Exit
        | _ ->
          decr tr_ind;
          Log.debugf 20 "Looking at trail element %d" (fun k->k !tr_ind);
          match Vec.get env.elt_queue !tr_ind with
          | Lit _ -> ()
          | Atom a ->
            begin match a.var.reason with
              | Some (Bcp d) ->
                let tmp, res = Proof.resolve (Proof.merge !c (Proof.to_list d)) in
                begin match tmp with
                  | [] -> ()
                  | [b] when b == a.var.pa ->
                    clause_bump_activity d;
                    var_bump_activity a.var;
                    history := d :: !history;
                    c := res
                  | _ -> assert false
                end
              | None | Some Decision | Some Semantic _ -> ()
            end
      done; assert false
    with Exit ->
      let learnt = List.fast_sort (
          fun a b -> Pervasives.compare b.var.v_level a.var.v_level) !c in
      let blevel = backtrack_lvl !is_uip learnt in
      blevel, learnt, List.rev !history, !is_uip

  let get_atom i =
    match Vec.get env.elt_queue i with
    | Lit _ -> assert false | Atom x -> x

  let analyze_sat c_clause =
    let pathC  = ref 0 in
    let learnt = ref [] in
    let cond   = ref true in
    let blevel = ref 0 in
    let seen   = ref [] in
    let c      = ref c_clause in
    let tr_ind = ref (Vec.size env.elt_queue - 1) in
    let size   = ref 1 in
    let history = ref [] in
    assert (decision_level () > 0);
    while !cond do
      begin match !c.cpremise with
        | History _ -> clause_bump_activity !c
        | Hyp _ | Lemma _ -> ()
      end;
      history := !c :: !history;
      (* visit the current predecessors *)
      for j = 0 to Array.length !c.atoms - 1 do
        let q = !c.atoms.(j) in
        assert (q.is_true || q.neg.is_true && q.var.v_level >= 0); (* Pas sur *)
        if q.var.v_level = 0 then begin
          assert (q.neg.is_true);
          match q.var.reason with
          | Some Bcp cl -> history := cl :: !history
          | _ -> assert false
        end;
        if not q.var.seen then begin
          q.var.seen <- true;
          seen := q :: !seen;
          if q.var.v_level > 0 then begin
            var_bump_activity q.var;
            if q.var.v_level >= decision_level () then begin
              incr pathC
            end else begin
              learnt := q :: !learnt;
              incr size;
              blevel := max !blevel q.var.v_level
            end
          end
        end
      done;

      (* look for the next node to expand *)
      while not (get_atom !tr_ind).var.seen do decr tr_ind done;
      decr pathC;
      let p = get_atom !tr_ind in
      decr tr_ind;
      match !pathC, p.var.reason with
      | 0, _ ->
        cond := false;
        learnt := p.neg :: (List.rev !learnt)
      | n, Some Bcp cl ->
        c := cl
      | n, _ -> assert false
    done;
    List.iter (fun q -> q.var.seen <- false) !seen;
    !blevel, !learnt, List.rev !history, true

  let analyze c_clause =
    if St.mcsat then
      analyze_mcsat c_clause
    else
      analyze_sat c_clause

  let record_learnt_clause confl blevel learnt history is_uip =
    begin match learnt with
      | [] -> assert false
      | [fuip] ->
        assert (blevel = 0);
        if fuip.neg.is_true then
          report_unsat confl
        else begin
          let name = fresh_lname () in
          let uclause = make_clause name learnt history in
          Vec.push env.clauses_learnt uclause;
          enqueue_bool fuip 0 (Bcp uclause)
        end
      | fuip :: _ ->
        let name = fresh_lname () in
        let lclause = make_clause name learnt history in
        Vec.push env.clauses_learnt lclause;
        attach_clause lclause;
        clause_bump_activity lclause;
        if is_uip then
          enqueue_bool fuip blevel (Bcp lclause)
        else begin
          env.next_decision <- Some fuip.neg
        end
    end;
    var_decay_activity ();
    clause_decay_activity ()

  let add_boolean_conflict confl =
    env.next_decision <- None;
    env.conflicts <- env.conflicts + 1;
    if decision_level() = 0 || Array_util.for_all (fun a -> a.var.v_level = 0) confl.atoms then
      report_unsat confl; (* Top-level conflict *)
    let blevel, learnt, history, is_uip = analyze confl in
    cancel_until blevel;
    record_learnt_clause confl blevel learnt (History history) is_uip

  (* Add a new clause *)
  let add_clause ?(force=false) init =
    Log.debugf 90 "Adding clause:@[<hov>%a@]" (fun k -> k St.pp_clause init);
    let vec = match init.cpremise with
      | Hyp _ -> env.clauses_hyps
      | Lemma _ -> env.clauses_learnt
      | History _ -> assert false
    in
    try
      let atoms, history = partition init.atoms in
      let clause =
        if history = [] then init
        else make_clause ?tag:init.tag (fresh_name ()) atoms (History (init :: history))
      in
      Log.debugf 4 "New clause:@ @[%a@]" (fun k->k St.pp_clause clause);
      Vec.push vec clause;
      match atoms with
      | [] ->
        report_unsat clause
      | a::b::_ ->
        if a.neg.is_true then begin
          Array.sort (fun a b ->
              compare b.var.v_level a.var.v_level) clause.atoms;
          attach_clause clause;
          add_boolean_conflict init
        end else begin
          attach_clause clause;
          if b.neg.is_true && not a.is_true && not a.neg.is_true then begin
            let lvl = List.fold_left (fun m a -> max m a.var.v_level) 0 atoms in
            cancel_until lvl;
            enqueue_bool a lvl (Bcp clause)
          end
        end
      | [a]   ->
        Log.debugf 5 "New unit clause, propagating : %a" (fun k->k St.pp_atom a);
        cancel_until 0;
        enqueue_bool a 0 (Bcp clause)
    with Trivial ->
      Vec.push vec init;
      Log.debugf 5 "Trivial clause ignored : @[%a@]" (fun k->k St.pp_clause init)

  let propagate_in_clause a c i watched new_sz =
    let atoms = c.atoms in
    let first = atoms.(0) in
    if first == a.neg then begin (* false lit must be at index 1 *)
      atoms.(0) <- atoms.(1);
      atoms.(1) <- first
    end;
    let first = atoms.(0) in
    if first.is_true then begin
      (* true clause, keep it in watched *)
      Vec.set watched !new_sz c;
      incr new_sz;
    end else
      try (* look for another watch lit *)
        for k = 2 to Array.length atoms - 1 do
          let ak = atoms.(k) in
          if not (ak.neg.is_true) then begin
            (* watch lit found: update and exit *)
            atoms.(1) <- ak;
            atoms.(k) <- a.neg;
            Vec.push ak.neg.watched c;
            raise Exit
          end
        done;
        (* no watch lit found *)
        if first.neg.is_true || (th_eval first = Some false) then begin
          (* clause is false *)
          env.elt_head <- Vec.size env.elt_queue;
          for k = i to Vec.size watched - 1 do
            Vec.set watched !new_sz (Vec.get watched k);
            incr new_sz;
          done;
          raise (Conflict c)
        end else begin
          (* clause is unit *)
          Vec.set watched !new_sz c;
          incr new_sz;
          enqueue_bool first (decision_level ()) (Bcp c)
        end
      with Exit -> ()

  let propagate_atom a res =
    let watched = a.watched in
    let new_sz_w = ref 0 in
    begin
      try
        for i = 0 to Vec.size watched - 1 do
          let c = Vec.get watched i in
          if c.attached then propagate_in_clause a c i watched new_sz_w
        done;
      with Conflict c ->
        assert (!res = None);
        res := Some c
    end;
    let dead_part = Vec.size watched - !new_sz_w in
    Vec.shrink watched dead_part

  (* Propagation (boolean and theory) *)
  let new_atom f =
    let a = atom f in
    ignore (th_eval a);
    a

  let slice_get i =
    match Vec.get env.elt_queue i with
    | Atom a ->
      Plugin_intf.Lit a.lit
    | Lit {l_level; term; assigned = Some v} ->
      Plugin_intf.Assign (term, v, l_level)
    | Lit _ -> assert false

  let slice_push l lemma =
    let atoms = List.rev_map (fun x -> new_atom x) l in
    Iheap.grow_to_by_double env.order (St.nb_elt ());
    List.iter (fun a -> insert_var_order (elt_of_var a.var)) atoms;
    let c = make_clause (fresh_tname ()) atoms (Lemma lemma) in
    Log.debugf 10 "Pushing clause %a" (fun k->k St.pp_clause c);
    Stack.push c env.clauses_pushed

  let do_push () =
    while not (Stack.is_empty env.clauses_pushed) do
      add_clause (Stack.pop env.clauses_pushed)
    done

  let slice_propagate f lvl =
    let a = atom f in
    Iheap.grow_to_by_double env.order (St.nb_elt ());
    enqueue_bool a lvl (Semantic lvl)

  let current_slice () = {
    Plugin_intf.start = env.th_head;
    length = (Vec.size env.elt_queue) - env.th_head;
    get = slice_get;
    push = slice_push;
    propagate = slice_propagate;
  }

  let full_slice () = {
    Plugin_intf.start = 0;
    length = Vec.size env.elt_queue;
    get = slice_get;
    push = slice_push;
    propagate = (fun _ -> assert false);
  }

  let rec theory_propagate () =
    assert (env.elt_head = Vec.size env.elt_queue);
    if env.th_head >= env.elt_head then
      None
    else begin
      let slice = current_slice () in
      env.th_head <- env.elt_head;
      match Plugin.assume slice with
      | Plugin_intf.Sat ->
        propagate ()
      | Plugin_intf.Unsat (l, p) ->
        let l = List.rev_map new_atom l in
        Iheap.grow_to_by_double env.order (St.nb_elt ());
        List.iter (fun a -> insert_var_order (elt_of_var a.var)) l;
        let c = St.make_clause (St.fresh_tname ()) l (Lemma p) in
        Some c
    end

  and propagate () =
    (* First, treat the pushed clause stack *)
    do_push ();
    (* Now, check that the situation is sane *)
    if env.elt_head > Vec.size env.elt_queue then
      assert false
    else if env.elt_head = Vec.size env.elt_queue then
      theory_propagate ()
    else begin
      let num_props = ref 0 in
      let res = ref None in
      while env.elt_head < Vec.size env.elt_queue do
        begin match Vec.get env.elt_queue env.elt_head with
          | Lit _ -> ()
          | Atom a ->
            incr num_props;
            propagate_atom a res
        end;
        env.elt_head <- env.elt_head + 1;
      done;
      env.propagations <- env.propagations + !num_props;
      env.simpDB_props <- env.simpDB_props - !num_props;
      match !res with
      | None -> theory_propagate ()
      | _ -> !res
    end

  (*
  (* heuristic comparison between clauses, by their size (unary/binary or not)
      and activity *)
  let f_sort_db c1 c2 =
    let sz1 = Vec.size c1.atoms in
    let sz2 = Vec.size c2.atoms in
    let c = compare c1.activity c2.activity in
    if sz1 = sz2 && c = 0 then 0
    else
    if sz1 > 2 && (sz2 = 2 || c < 0) then -1
    else 1

  (* returns true if the clause is used as a reason for a propagation,
        and therefore can be needed in case of conflict. In this case
        the clause can't be forgotten *)
  let locked c = false (*
    Vec.exists
      (fun v -> match v.reason with
         | Some c' -> c ==c'
         | _ -> false
      ) env.vars
      *)
  *)

  (* remove some learnt clauses *)
  let reduce_db () = () (*
    let extra_lim = env.clause_inc /. (to_float (Vec.size env.learnts)) in
    Vec.sort env.learnts f_sort_db;
    let lim2 = Vec.size env.learnts in
    let lim1 = lim2 / 2 in
    let j = ref 0 in
    for i = 0 to lim1 - 1 do
      let c = Vec.get env.learnts i in
      if Vec.size c.atoms > 2 && not (locked c) then
        detach_clause c
      else
        begin Vec.set env.learnts !j c; incr j end
    done;
    for i = lim1 to lim2 - 1 do
      let c = Vec.get env.learnts i in
      if Vec.size c.atoms > 2 && not (locked c) && c.activity < extra_lim then
        detach_clause c
      else
        begin Vec.set env.learnts !j c; incr j end
    done;
    Vec.shrink env.learnts (lim2 - !j)
    *)

(*
  (* remove from [vec] the clauses that are satisfied in the current trail *)
  let remove_satisfied vec =
    for i = 0 to Vec.size vec - 1 do
      let c = Vec.get vec i in
      if satisfied c then detach_clause c
    done

  let simplify () =
    assert (decision_level () = 0);
    if is_unsat () then raise Unsat;
    begin
      match propagate () with
      | Some confl -> report_unsat confl
      | None -> ()
    end;
    if Vec.size env.elt_queue <> env.simpDB_assigns && env.simpDB_props <= 0 then begin
      if Vec.size env.clauses_learnt > 0 then remove_satisfied env.clauses_learnt;
      if env.remove_satisfied then remove_satisfied env.clauses_hyps;
      (*Iheap.filter env.order f_filter f_weight;*)
      env.simpDB_assigns <- Vec.size env.elt_queue;
      env.simpDB_props <- env.clauses_literals + env.learnts_literals;
    end
*)

  (* Decide on a new litteral *)
  let rec pick_branch_aux atom =
    let v = atom.var in
    if v.v_level >= 0 then begin
      assert (v.pa.is_true || v.na.is_true);
      pick_branch_lit ()
    end else match Plugin.eval atom.lit with
      | Plugin_intf.Unknown ->
        env.decisions <- env.decisions + 1;
        new_decision_level();
        let current_level = decision_level () in
        enqueue_bool atom current_level Decision
      | Plugin_intf.Valued (b, lvl) ->
        let a = if b then atom else atom.neg in
        enqueue_bool a lvl (Semantic lvl)

  and pick_branch_lit () =
    match env.next_decision with
    | Some atom ->
      env.next_decision <- None;
      pick_branch_aux atom
    | None ->
      begin try
          begin match St.get_elt (Iheap.remove_min f_weight env.order) with
            | E_lit l ->
              if l.l_level >= 0 then
                pick_branch_lit ()
              else begin
                let value = Plugin.assign l.term in
                env.decisions <- env.decisions + 1;
                new_decision_level();
                let current_level = decision_level () in
                enqueue_assign l value current_level
              end
            | E_var v ->
              pick_branch_aux v.pa
          end
        with Not_found -> raise Sat
      end

  let search n_of_conflicts n_of_learnts =
    let conflictC = ref 0 in
    env.starts <- env.starts + 1;
    while true do
      match propagate () with
      | Some confl -> (* Conflict *)
        incr conflictC;
        add_boolean_conflict confl

      | None -> (* No Conflict *)
        assert (env.elt_head = Vec.size env.elt_queue);
        if Vec.size env.elt_queue = St.nb_elt () (* env.nb_init_vars *) then raise Sat;
        if n_of_conflicts > 0 && !conflictC >= n_of_conflicts then begin
          env.progress_estimate <- progress_estimate();
          cancel_until 0;
          raise Restart
        end;
        (* if decision_level() = 0 then simplify (); *)

        if n_of_learnts >= 0 &&
           Vec.size env.clauses_learnt - Vec.size env.elt_queue >= n_of_learnts then
          reduce_db();

        pick_branch_lit ()
    done

  let check_clause c =
    let b = ref false in
    let atoms = c.atoms in
    for i = 0 to Array.length atoms - 1 do
      b := !b || atoms.(i).is_true
    done;
    assert (!b)

  let check_vec vec =
    for i = 0 to Vec.size vec - 1 do check_clause (Vec.get vec i) done

  let add_clauses ?tag cnf =
    let aux cl =
      let c = make_clause ?tag (fresh_hname ())
          cl (Hyp (current_level ())) in
      add_clause c;
      (* Clauses can be added after search has begun (and thus we are not at level 0,
         so better not do anything at this point.
         match propagate () with
         | None -> () | Some confl -> report_unsat confl
      *)
    in
    List.iter aux cnf

  (* fixpoint of propagation and decisions until a model is found, or a
     conflict is reached *)
  let solve () =
    if is_unsat () then raise Unsat;
    let n_of_conflicts = ref (to_float env.restart_first) in
    let n_of_learnts = ref ((to_float (nb_clauses())) *. env.learntsize_factor) in
    try
      while true do
        begin try
            search (to_int !n_of_conflicts) (to_int !n_of_learnts)
          with
          | Restart ->
            n_of_conflicts := !n_of_conflicts *. env.restart_inc;
            n_of_learnts   := !n_of_learnts *. env.learntsize_inc
          | Sat ->
            assert (env.elt_head = Vec.size env.elt_queue);
            Plugin.if_sat (full_slice ());
            do_push ();
            if is_unsat () then raise Unsat
            else if env.elt_head = Vec.size env.elt_queue (* sanity check *)
                 && env.elt_head = St.nb_elt () (* this is the important test to know if the search is finished *)  then
              raise Sat
        end
      done
    with
    | Sat -> ()

  let init_solver ?tag cnf =
    let nbv = St.nb_elt () in
    let nbc = env.nb_init_clauses + List.length cnf in
    Iheap.grow_to_by_double env.order nbv;
    (* List.iter (List.iter (fun a -> insert_var_order a.var)) cnf; *)
    St.iter_elt insert_var_order;
    Vec.grow_to_by_double env.clauses_hyps nbc;
    Vec.grow_to_by_double env.clauses_learnt nbc;
    env.nb_init_clauses <- nbc;
    add_clauses ?tag cnf

  let assume ?tag cnf =
    let cnf = List.rev_map (List.rev_map atom) cnf in
    init_solver ?tag cnf

  let eval_level lit =
    let var, negated = make_boolean_var lit in
    if not var.pa.is_true && not var.na.is_true
    then raise UndecidedLit
    else assert (var.v_level >= 0);
    let truth = var.pa.is_true in
    let value = match negated with
      | Formula_intf.Negated -> not truth
      | Formula_intf.Same_sign -> truth
    in
    value, var.v_level

  let eval lit = fst (eval_level lit)

  let hyps () = env.clauses_hyps

  let history () = env.clauses_learnt

  let unsat_conflict () = env.unsat_conflict

  let model () =
    let opt = function Some a -> a | None -> assert false in
    Vec.fold (fun acc e -> match e with
        | Lit v -> (v.term, opt v.assigned)  :: acc
        | Atom _ -> acc
      ) [] env.elt_queue

  (* Backtrack to decision_level 0, with trail_lim && theory env specified *)
  let reset_until push_lvl elt_lvl th_lvl th_env =
    Log.debug 1 "Resetting to decision level 0 (pop/forced)";
    env.th_head <- th_lvl;
    env.elt_head <- elt_lvl;
    for c = env.elt_head to Vec.size env.elt_queue - 1 do
      match Vec.get env.elt_queue c with
      | Lit l ->
        l.assigned <- None;
        l.l_level <- -1;
        insert_var_order (elt_of_lit l)
      | Atom a ->
        begin match a.var.reason with
          | Some Bcp { c_level } when c_level > push_lvl ->
            a.is_true <- false;
            a.neg.is_true <- false;
            a.var.v_level <- -1;
            a.var.reason <- None;
            insert_var_order (elt_of_var a.var)
          | _ ->
            if a.var.v_level = 0 then begin
              Vec.set env.elt_queue env.elt_head (of_atom a);
              env.elt_head <- env.elt_head + 1
            end else begin
              a.is_true <- false;
              a.neg.is_true <- false;
              a.var.v_level <- -1;
              a.var.reason <- None;
              insert_var_order (elt_of_var a.var)
            end
        end
    done;
    Plugin.backtrack th_env; (* recover the right theory env *)
    Vec.shrink env.elt_queue ((Vec.size env.elt_queue) - env.elt_head);
    Vec.clear env.elt_levels;
    Vec.clear env.th_levels;
    assert (Vec.size env.elt_levels = Vec.size env.th_levels);
    assert (env.elt_head = Vec.size env.elt_queue);
    ()

  let pop l =
    (* Check sanity of pop *)
    if l > current_level () then invalid_arg "cannot pop to level, it is too high"
    else if l < current_level () then begin

      let ul = Vec.get env.user_levels l in
      Vec.shrink env.user_levels (max 0 (Vec.size env.user_levels - l - 1));

      (* It is quite hard to check wether unsat status can be kept, so in doubt, we remove it *)
      env.unsat_conflict <- None;

      (* Backtrack to the level 0 with appropriate settings *)
      reset_until l ul.ul_elt_lvl ul.ul_th_lvl ul.ul_th_env;

      (* Log current assumptions for debugging purposes *)
      Log.debugf 99 "@[<v2>Current trail:@ %a@]"
        (fun k->
           let pp out () =
             for i = 0 to Vec.size env.elt_queue - 1 do
               Format.fprintf out "%s%s%d -- %a@,"
                 (if i = ul.ul_elt_lvl then "*" else " ")
                 (if i = ul.ul_th_lvl then "*" else " ")
                 i (fun fmt e ->
                     match e with
                     | Lit l -> St.pp_lit fmt l
                     | Atom a -> St.pp_atom fmt a)
                 (Vec.get env.elt_queue i)
             done
           in
           k pp ());

      (* Clear hypothesis not valid anymore *)
      for i = ul.ul_clauses to Vec.size env.clauses_hyps - 1 do
        let c = Vec.get env.clauses_hyps i in
        assert (c.c_level > l);
        detach_clause c
      done;
      Vec.shrink env.clauses_hyps (Vec.size env.clauses_hyps - ul.ul_clauses);

      (* Refresh the known tautologies simplified because of clauses that have been removed *)
      let s = Stack.create () in
      let new_sz = ref ul.ul_learnt in
      for i = ul.ul_learnt to Vec.size env.clauses_learnt - 1 do
        let c = Vec.get env.clauses_learnt i in
        if c.c_level > l then begin
          detach_clause c;
          match c.cpremise with
          | Lemma _ -> Stack.push c s
          | History ({ cpremise = Lemma _ } as c' :: _ ) -> Stack.push c' s
          | _ -> () (* Only simplified clauses can have a level > 0 *)
        end else begin
          Log.debugf 15 "Keeping intact clause %a" (fun k->k St.pp_clause c);
          Vec.set env.clauses_learnt !new_sz c;
          incr new_sz
        end
      done;
      Vec.shrink env.clauses_learnt (Vec.size env.clauses_learnt - !new_sz);
      Stack.iter (add_clause ~force:true) s
    end

  let reset () = pop base_level

end

