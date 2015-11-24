(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module Make (L : Log_intf.S)(St : Solver_types.S)
    (Th : Plugin_intf.S with type term = St.term and type formula = St.formula and type proof = St.proof) = struct

  module Proof = Res.Make(L)(St)

  open St

  exception Sat
  exception Unsat
  exception Restart
  exception Conflict of clause

  (* a push/pop state *)
  type user_level = {
    (* User levels always refer to decision_level 0 *)
    ul_trail : int; (* Number of atoms in trail at decision level 0 *)
    ul_th_env : Th.level; (* Theory state at level 0 *)
    ul_clauses : int; (* number of clauses *)
    ul_learnt : int; (* number of learnt clauses *)
    ul_proof_lvl : int; (* push/pop index for Res module *)
  }

  (* Singleton type containing the current state *)
  type env = {

    clauses_hyps : clause Vec.t;
    (* all currently active clauses *)
    clauses_learnt : clause Vec.t;
    (* learnt clauses  *)

    mutable unsat_conflict : clause option;
    (* conflict clause at decision level 0, if any *)



    elt_queue : t Vec.t;
    (* decision stack + propagated elements (atoms or assignments) *)

    elt_levels : int Vec.t;
    (* decision levels in [trail]  *)
    th_levels : Th.level Vec.t;
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

  let env = {
    unsat_conflict = None;

    clauses_hyps = Vec.make 0 dummy_clause;
    clauses_learnt = Vec.make 0 dummy_clause;

    th_head = 0;
    elt_head = 0;

    elt_queue = Vec.make 601 (of_atom dummy_atom);
    elt_levels = Vec.make 601 (-1);
    th_levels = Vec.make 100 Th.dummy;

    user_levels = Vec.make 20 {
        ul_trail = 0;
        ul_learnt = 0;
        ul_clauses = 0;
        ul_th_env = Th.dummy;
        ul_proof_lvl = -1;
      };

    order = Iheap.init 0;

    var_incr = 1.;
    clause_incr = 1.;
    var_decay = 1. /. 0.95;
    clause_decay = 1. /. 0.999;

    simpDB_assigns = -1;
    simpDB_props = 0;

    progress_estimate = 0.;
    remove_satisfied = true;

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

  let is_unsat () = match env.unsat_conflict with Some _ -> true | None -> false

  (* Level for push/pop operations *)
  type level = int

  let base_level = 0

  let current_level () = Vec.size env.user_levels

  (* Iteration over subterms *)
  module Mi = Map.Make(struct type t = int let compare = Pervasives.compare end)
  let iter_map = ref Mi.empty

  let iter_sub f v =
    try
      List.iter f (Mi.find v.vid !iter_map)
    with Not_found ->
      let l = ref [] in
      Th.iter_assignable (fun t -> l := add_term t :: !l) v.pa.lit;
      iter_map := Mi.add v.vid !l !iter_map;
      List.iter f !l

  let atom lit =
    let res = add_atom lit in
    iter_sub ignore res.var;
    res

  (* Misc functions *)
  let to_float i = float_of_int i
  let to_int f = int_of_float f

  (* Accessors for litterals *)
  let f_weight i j =
    get_elt_weight (St.get_elt j) < get_elt_weight (St.get_elt i)

  let f_filter i =
    get_elt_level (St.get_elt i) < 0

  (* Var/clause activity *)
  let insert_var_order e = destruct_elt e
      (fun v -> Iheap.insert f_weight env.order v.lid)
      (fun v ->
          Iheap.insert f_weight env.order v.vid;
          iter_sub (fun t -> Iheap.insert f_weight env.order t.lid) v
      )

  let var_decay_activity () =
    env.var_incr <- env.var_incr *. env.var_decay

  let clause_decay_activity () =
    env.clause_incr <- env.clause_incr *. env.clause_decay

  let var_bump_activity_aux v =
    v.weight <- v.weight +. env.var_incr;
    if v.weight > 1e100 then begin
      for i = 0 to (St.nb_elt ()) - 1 do
        set_elt_weight (St.get_elt i) ((get_elt_weight (St.get_elt i)) *. 1e-100)
      done;
      env.var_incr <- env.var_incr *. 1e-100;
    end;
    if Iheap.in_heap env.order v.vid then
      Iheap.decrease f_weight env.order v.vid

  let lit_bump_activity_aux (v : lit) =
    v.weight <- v.weight +. env.var_incr;
    if v.weight > 1e100 then begin
      for i = 0 to (St.nb_elt ()) - 1 do
        set_elt_weight (St.get_elt i) ((get_elt_weight (St.get_elt i)) *. 1e-100)
      done;
      env.var_incr <- env.var_incr *. 1e-100;
    end;
    if Iheap.in_heap env.order v.lid then
      Iheap.decrease f_weight env.order v.lid

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

  (* Convenient access *)
  let decision_level () = Vec.size env.elt_levels

  let nb_assigns () = Vec.size env.elt_queue
  let nb_clauses () = Vec.size env.clauses_hyps
  let nb_learnts () = Vec.size env.clauses_learnt
  let nb_vars    () = St.nb_elt ()

  (* Manipulating decision levels *)
  let new_decision_level() =
    Vec.push env.elt_levels (Vec.size env.elt_queue);
    Vec.push env.th_levels (Th.current_level ()); (* save the current tenv *)
    L.debug 5 "New decision level : %d (%d in env queue)(%d in trail)"
      (Vec.size env.elt_levels) (Vec.size env.th_levels) (Vec.size env.elt_queue);
    ()

  (* Adding/removing clauses *)
  let attach_clause c =
    Vec.push (Vec.get c.atoms 0).neg.watched c;
    Vec.push (Vec.get c.atoms 1).neg.watched c;
    L.debug 8 "%a <-- %a" St.pp_atom (Vec.get c.atoms 0).neg St.pp_clause c;
    L.debug 8 "%a <-- %a" St.pp_atom (Vec.get c.atoms 1).neg St.pp_clause c;
    if c.learnt then
      env.learnts_literals <- env.learnts_literals + Vec.size c.atoms
    else
      env.clauses_literals <- env.clauses_literals + Vec.size c.atoms

  let detach_clause c =
    L.debug 15 "Removing clause %a" St.pp_clause c;
    c.removed <- true;
        (* Not necessary, cleanup is done during propagation
        Vec.remove (Vec.get c.atoms 0).neg.watched c;
        Vec.remove (Vec.get c.atoms 1).neg.watched c;
        *)
    if c.learnt then
      env.learnts_literals <- env.learnts_literals - Vec.size c.atoms
    else
      env.clauses_literals <- env.clauses_literals - Vec.size c.atoms

  let remove_clause c = detach_clause c

  let satisfied c =
    Vec.exists (fun atom -> atom.is_true) c.atoms

  (* cancel down to [lvl] excluded *)
  let cancel_until lvl =
    L.debug 1 "Backtracking to decision level %d (excluded)" lvl;
    if decision_level () > lvl then begin
      env.elt_head <- Vec.get env.elt_levels lvl;
      env.th_head <- env.elt_head;
      for c = env.elt_head to Vec.size env.elt_queue - 1 do
        destruct (Vec.get env.elt_queue c)
        (fun v ->
            v.assigned <- None;
            v.level <- -1;
            insert_var_order (elt_of_lit v)
        )
        (fun a ->
            if a.var.level <= lvl then begin
              Vec.set env.elt_queue env.elt_head (of_atom a);
              env.elt_head <- env.elt_head + 1
            end else begin
              a.is_true <- false;
              a.neg.is_true <- false;
              a.var.level <- -1;
              a.var.reason <- Bcp None;
              insert_var_order (elt_of_var a.var)
            end)
      done;
      Th.backtrack (Vec.get env.th_levels lvl); (* recover the right tenv *)
      Vec.shrink env.elt_queue ((Vec.size env.elt_queue) - env.elt_head);
      Vec.shrink env.elt_levels ((Vec.size env.elt_levels) - lvl);
      Vec.shrink env.th_levels ((Vec.size env.th_levels) - lvl);
    end;
    assert (Vec.size env.elt_levels = Vec.size env.th_levels);
    ()

  let report_unsat ({atoms=atoms} as confl) =
    L.debug 5 "Unsat conflict : %a" St.pp_clause confl;
    env.unsat_conflict <- Some confl;
    raise Unsat

  let enqueue_bool a lvl reason =
    assert (not a.neg.is_true);
    if a.is_true then
      L.debug 10 "Litteral %a already in queue" pp_atom a
    else begin
      assert (a.var.level < 0 && a.var.reason = Bcp None && lvl >= 0);
      a.is_true <- true;
      a.var.level <- lvl;
      a.var.reason <- reason;
      Vec.push env.elt_queue (of_atom a);
      L.debug 2 "Enqueue (%d): %a" (nb_assigns ()) pp_atom a
    end

  let enqueue_assign v value lvl =
    v.assigned <- Some value;
    v.level <- lvl;
    Vec.push env.elt_queue (of_lit v);
    L.debug 2 "Enqueue (%d): %a" (nb_assigns ()) St.pp_lit v

  let th_eval a =
    if a.is_true || a.neg.is_true then None
    else match Th.eval a.lit with
    | Th.Unknown -> None
    | Th.Valued (b, lvl) ->
      let atom = if b then a else a.neg in
      enqueue_bool atom lvl (Semantic lvl);
      Some b

  (* conflict analysis *)
  let max_lvl_atoms l =
      List.fold_left (fun (max_lvl, acc) a ->
          if a.var.level = max_lvl then (max_lvl, a :: acc)
          else if a.var.level > max_lvl then (a.var.level, [a])
          else (max_lvl, acc)) (0, []) l

  let backtrack_lvl is_uip = function
    | [] -> 0
    | a :: r when not is_uip -> max (a.var.level - 1) 0
    | a :: [] -> 0
    | a :: b :: r -> assert(a.var.level <> b.var.level); b.var.level

  let analyze_mcsat c_clause =
    let tr_ind  = ref (Vec.size env.elt_queue) in
    let is_uip  = ref false in
    let c       = ref (Proof.to_list c_clause) in
    let history = ref [c_clause] in
    let c_level = ref 0 in
    clause_bump_activity c_clause;
    let is_semantic a = match a.var.reason with
        | Semantic _ -> true
        | _ -> false
    in
    try while true do
        let lvl, atoms = max_lvl_atoms !c in
        L.debug 15 "Current conflict clause :";
        List.iter (fun a -> L.debug 15 " |- %a" St.pp_atom a) !c;
        if lvl = 0 then raise Exit;
        match atoms with
        | [] | _ :: [] ->
                L.debug 15 "Found UIP clause";
                is_uip := true;
                raise Exit
        | _ when List.for_all is_semantic atoms ->
                L.debug 15 "Found Semantic backtrack clause";
                raise Exit
        | _ ->
                decr tr_ind;
                L.debug 20 "Looking at trail element %d" !tr_ind;
                destruct (Vec.get env.elt_queue !tr_ind)
                (fun v -> L.debug 15 "%a" St.pp_lit v)
                (fun a -> match a.var.reason with
                    | Bcp (Some d) ->
                            L.debug 15 "Propagation : %a" St.pp_atom a;
                            L.debug 15 " |- %a" St.pp_clause d;
                            let tmp, res = Proof.resolve (Proof.merge !c (Proof.to_list d)) in
                            begin match tmp with
                            | [] -> L.debug 15 "No lit to resolve over."
                            | [b] when b == a.var.pa ->
                                    c_level := max !c_level d.c_level;
                                    clause_bump_activity d;
                                    var_bump_activity a.var;
                                    history := d :: !history;
                                    c := res
                            | _ -> assert false
                            end
                    | Bcp None -> L.debug 15 "Decision : %a" St.pp_atom a
                    | Semantic _ -> L.debug 15 "Semantic propagation : %a" St.pp_atom a)
    done; assert false
    with Exit ->
      let learnt = List.sort (fun a b -> Pervasives.compare b.var.level a.var.level) !c in
      let blevel = backtrack_lvl !is_uip learnt in
      blevel, learnt, !history, !is_uip, !c_level

  let get_atom i =
    destruct (Vec.get env.elt_queue i)
      (fun _ -> assert false) (fun x -> x)

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
    let c_level = ref 0 in
    while !cond do
      if !c.learnt then clause_bump_activity !c;
      history := !c :: !history;
      (* visit the current predecessors *)
      for j = 0 to Vec.size !c.atoms - 1 do
        let q = Vec.get !c.atoms j in
        assert (q.is_true || q.neg.is_true && q.var.level >= 0); (* Pas sur *)
        if not q.var.seen && q.var.level > 0 then begin
          var_bump_activity q.var;
          q.var.seen <- true;
          seen := q :: !seen;
          if q.var.level >= decision_level () then begin
            incr pathC
          end else begin
            learnt := q :: !learnt;
            incr size;
            blevel := max !blevel q.var.level
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
      | n, Bcp Some cl ->
        c_level := max !c_level cl.c_level;
        c := cl
      | n, _ -> assert false
    done;
    List.iter (fun q -> q.var.seen <- false) !seen;
    !blevel, !learnt, !history, true, !c_level

  let analyze c_clause =
    if St.mcsat then
      analyze_mcsat c_clause
    else
      analyze_sat c_clause

  let record_learnt_clause confl blevel learnt history is_uip lvl =
    begin match learnt with
      | [] -> assert false
      | [fuip] ->
        assert (blevel = 0);
        if fuip.neg.is_true then
          report_unsat confl
        else begin
          let name = fresh_lname () in
          let uclause = make_clause name learnt (List.length learnt) true history lvl in
          L.debug 1 "Unit clause learnt : %a" St.pp_clause uclause;
          Vec.push env.clauses_learnt uclause;
          enqueue_bool fuip 0 (Bcp (Some uclause))
        end
      | fuip :: _ ->
        let name = fresh_lname () in
        let lclause = make_clause name learnt (List.length learnt) true history lvl in
        L.debug 2 "New clause learnt : %a" St.pp_clause lclause;
        Vec.push env.clauses_learnt lclause;
        attach_clause lclause;
        clause_bump_activity lclause;
        if is_uip then
          enqueue_bool fuip blevel (Bcp (Some lclause))
        else begin
          env.decisions <- env.decisions + 1;
          new_decision_level();
          enqueue_bool fuip.neg (decision_level ()) (Bcp None)
        end
    end;
    var_decay_activity ();
    clause_decay_activity ()

  let add_boolean_conflict confl =
    env.conflicts <- env.conflicts + 1;
    if decision_level() = 0 || Vec.for_all (fun a -> a.var.level = 0) confl.atoms then
      report_unsat confl; (* Top-level conflict *)
    let blevel, learnt, history, is_uip, lvl = analyze confl in
    cancel_until blevel;
    record_learnt_clause confl blevel learnt (History history) is_uip lvl

  (* Add a new clause *)
  exception Trivial

  let simplify_zero atoms level0 =
    assert (decision_level () = 0);
    let aux (atoms, init, lvl) a =
      if a.is_true then raise Trivial;
      if a.neg.is_true then begin
        match a.var.reason with
        | Bcp (Some cl) -> atoms, false, max lvl cl.c_level
        | Semantic 0 -> atoms, init, lvl
        | _ -> assert false
      end else
        a::atoms, init, lvl
    in
    let atoms, init, lvl = List.fold_left aux ([], true, level0) atoms in
    List.fast_sort (fun a b -> a.var.vid - b.var.vid) atoms, init, lvl

  let partition atoms init0 =
    let rec partition_aux trues unassigned falses init lvl = function
      | [] -> trues @ unassigned @ falses, init, lvl
      | a :: r ->
        if a.is_true then
          if a.var.level = 0 then raise Trivial
          else (a::trues) @ unassigned @ falses @ r, init, lvl
        else if a.neg.is_true then
          if a.var.level = 0 then begin
            match a.var.reason with
            | Bcp (Some cl) ->
              partition_aux trues unassigned falses false (max lvl cl.c_level) r
            | Semantic 0 ->
              partition_aux trues unassigned falses init lvl r
            | _ -> assert false
          end else
            partition_aux trues unassigned (a::falses) init lvl r
        else
          partition_aux trues (a::unassigned) falses init lvl r
    in
    if decision_level () = 0 then
      simplify_zero atoms init0
    else
      partition_aux [] [] [] false init0 atoms

  let add_clause ?(force=false) init0 =
    let vec = match init0.cpremise with
      | Lemma _ -> env.clauses_learnt
      | History [] -> env.clauses_hyps
      | History _ -> assert false
    in
    L.debug 10 "Adding clause : %a" St.pp_clause init0;
    try
      if not force && Proof.has_been_proved init0 then raise Trivial;
      if not (Proof.is_proven init0) then assert false; (* Important side-effect, DO NOT REMOVE *)
      let atoms, init, level = partition (Vec.to_list init0.atoms) init0.c_level in
      let size = List.length atoms in
      match atoms with
      | [] ->
        report_unsat init0;
      | a::b::_ ->
        let clause =
          if init then init0
          else make_clause ?tag:init0.tag (fresh_name ()) atoms size true (History [init0]) level
        in
        L.debug 1 "New clause : %a" St.pp_clause clause;
        attach_clause clause;
        Vec.push vec clause;
        if a.neg.is_true then begin
          let lvl = List.fold_left (fun m a -> max m a.var.level) 0 atoms in
          cancel_until lvl;
          add_boolean_conflict clause
        end else if b.neg.is_true && not a.is_true && not a.neg.is_true then begin
          let lvl = List.fold_left (fun m a -> max m a.var.level) 0 atoms in
          cancel_until lvl;
          enqueue_bool a lvl (Bcp (Some clause))
        end
      | [a]   ->
        L.debug 1 "New unit clause, propagating : %a" St.pp_atom a;
        cancel_until 0;
        enqueue_bool a 0 (Bcp (Some init0))
    with Trivial -> L.debug 1 "Trivial clause ignored"

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

  let propagate_in_clause a c i watched new_sz =
    let atoms = c.atoms in
    let first = Vec.get atoms 0 in
    if first == a.neg then begin (* false lit must be at index 1 *)
      Vec.set atoms 0 (Vec.get atoms 1);
      Vec.set atoms 1 first
    end;
    let first = Vec.get atoms 0 in
    if first.is_true then begin
      (* true clause, keep it in watched *)
      Vec.set watched !new_sz c;
      incr new_sz;
    end
    else
      try (* look for another watch lit *)
        for k = 2 to Vec.size atoms - 1 do
          let ak = Vec.get atoms k in
          if not (ak.neg.is_true) then begin
            (* watch lit found: update and exit *)
            Vec.set atoms 1 ak;
            Vec.set atoms k a.neg;
            Vec.push ak.neg.watched c;
            L.debug 8 "New watcher (%a) for clause : %a" St.pp_atom ak.neg St.pp_clause c;
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
          L.debug 3 "Conflict found : %a" St.pp_clause c;
          raise (Conflict c)
        end else begin
          (* clause is unit *)
          Vec.set watched !new_sz c;
          incr new_sz;
          L.debug 5 "Unit clause : %a" St.pp_clause c;
          enqueue_bool first (decision_level ()) (Bcp (Some c))
        end
      with Exit -> ()

  let propagate_atom a res =
    L.debug 8 "Propagating %a" St.pp_atom a;
    let watched = a.watched in
    L.debug 10 "Watching %a :" St.pp_atom a;
    Vec.iter (fun c -> L.debug 10 "  %a" St.pp_clause c) watched;
    let new_sz_w = ref 0 in
    begin
      try
        for i = 0 to Vec.size watched - 1 do
          let c = Vec.get watched i in
          if not c.removed then propagate_in_clause a c i watched new_sz_w
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
    L.debug 10 "New atom : %a" St.pp_atom a;
    ignore (th_eval a);
    a

  let slice_get i = destruct (Vec.get env.elt_queue i)
      (function {level; term; assigned = Some v} -> Th.Assign (term, v), level | _ -> assert false)
      (fun a -> Th.Lit a.lit, a.var.level)

  let slice_push l lemma =
    let atoms = List.rev_map (fun x -> new_atom x) l in
    Iheap.grow_to_by_double env.order (St.nb_elt ());
    List.iter (fun a -> insert_var_order (elt_of_var a.var)) atoms;
    let c = make_clause (fresh_tname ()) atoms (List.length atoms) true (Lemma lemma) base_level in
    add_clause c

  let slice_propagate f lvl =
    let a = atom f in
    Iheap.grow_to_by_double env.order (St.nb_elt ());
    enqueue_bool a lvl (Semantic lvl)

  let current_slice () = Th.({
      start = env.th_head;
      length = (Vec.size env.elt_queue) - env.th_head;
      get = slice_get;
      push = slice_push;
      propagate = slice_propagate;
    })

  let full_slice tag = Th.({
      start = 0;
      length = Vec.size env.elt_queue;
      get = slice_get;
      push = (fun cl proof -> tag := true; slice_push cl proof);
      propagate = (fun _ -> assert false);
    })

  let rec theory_propagate () =
    let slice = current_slice () in
    env.th_head <- nb_assigns ();
    match Th.assume slice with
    | Th.Sat ->
      propagate ()
    | Th.Unsat (l, p) ->
      let l = List.rev_map new_atom l in
      Iheap.grow_to_by_double env.order (St.nb_elt ());
      List.iter (fun a -> insert_var_order (elt_of_var a.var)) l;
      let c = St.make_clause (St.fresh_tname ()) l (List.length l) true (Lemma p) base_level in
      Some c

  and propagate () =
    if env.elt_head > Vec.size env.elt_queue then
      assert false
    else if env.elt_head = Vec.size env.elt_queue then
      None
    else begin
      let num_props = ref 0 in
      let res = ref None in
      while env.elt_head < Vec.size env.elt_queue do
        destruct (Vec.get env.elt_queue env.elt_head)
          (fun a -> ())
          (fun a ->
             incr num_props;
             propagate_atom a res);
        env.elt_head <- env.elt_head + 1
      done;
      env.propagations <- env.propagations + !num_props;
      env.simpDB_props <- env.simpDB_props - !num_props;
      match !res with
      | None -> theory_propagate ()
      | _ -> !res
    end

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
        remove_clause c
      else
        begin Vec.set env.learnts !j c; incr j end
    done;
    for i = lim1 to lim2 - 1 do
      let c = Vec.get env.learnts i in
      if Vec.size c.atoms > 2 && not (locked c) && c.activity < extra_lim then
        remove_clause c
      else
        begin Vec.set env.learnts !j c; incr j end
    done;
    Vec.shrink env.learnts (lim2 - !j)
    *)

  (* remove from [vec] the clauses that are satisfied in the current trail *)
  let remove_satisfied vec =
    for i = 0 to Vec.size vec - 1 do
      let c = Vec.get vec i in
      if satisfied c then remove_clause c
    done

  module HUC = Hashtbl.Make
      (struct type t = clause let equal = (==) let hash = Hashtbl.hash end)

  let simplify () =
    assert (decision_level () = 0);
    if is_unsat () then raise Unsat;
    begin
      match propagate () with
      | Some confl -> report_unsat confl
      | None -> ()
    end;
    if nb_assigns() <> env.simpDB_assigns && env.simpDB_props <= 0 then begin
      if Vec.size env.clauses_learnt > 0 then remove_satisfied env.clauses_learnt;
      if env.remove_satisfied then remove_satisfied env.clauses_hyps;
      (*Iheap.filter env.order f_filter f_weight;*)
      env.simpDB_assigns <- nb_assigns ();
      env.simpDB_props <- env.clauses_literals + env.learnts_literals;
    end

  (* Decide on a new litteral *)
  let rec pick_branch_lit () =
    let max = Iheap.remove_min f_weight env.order in
    destruct_elt (St.get_elt max)
      (fun v ->
         if v.level >= 0 then
           pick_branch_lit ()
         else begin
           let value = Th.assign v.term in
           env.decisions <- env.decisions + 1;
           new_decision_level();
           let current_level = decision_level () in
           L.debug 5 "Deciding on %a" St.pp_lit v;
           enqueue_assign v value current_level
         end)
      (fun v ->
         if v.level >= 0 then begin
           assert (v.pa.is_true || v.na.is_true);
           pick_branch_lit ()
         end else match Th.eval v.pa.lit with
           | Th.Unknown ->
             env.decisions <- env.decisions + 1;
             new_decision_level();
             let current_level = decision_level () in
             L.debug 5 "Deciding on %a" St.pp_atom v.pa;
             enqueue_bool v.pa current_level (Bcp None)
           | Th.Valued (b, lvl) ->
             let a = if b then v.pa else v.na in
             enqueue_bool a lvl (Semantic lvl))

  let search n_of_conflicts n_of_learnts =
    let conflictC = ref 0 in
    env.starts <- env.starts + 1;
    while (true) do
      match propagate () with
      | Some confl -> (* Conflict *)
        incr conflictC;
        add_boolean_conflict confl

      | None -> (* No Conflict *)
        if nb_assigns() = St.nb_elt () (* env.nb_init_vars *) then raise Sat;
        if n_of_conflicts > 0 && !conflictC >= n_of_conflicts then begin
          L.debug 1 "Restarting...";
          env.progress_estimate <- progress_estimate();
          cancel_until 0;
          raise Restart
        end;
        if decision_level() = 0 then simplify ();

        if n_of_learnts >= 0 &&
           Vec.size env.clauses_learnt - nb_assigns() >= n_of_learnts then
          reduce_db();

        pick_branch_lit ()
    done

  let check_clause c =
    let b = ref false in
    let atoms = c.atoms in
    for i = 0 to Vec.size atoms - 1 do
      let a = Vec.get atoms i in
      b := !b || a.is_true
    done;
    assert (!b)

  let check_vec vec =
    for i = 0 to Vec.size vec - 1 do check_clause (Vec.get vec i) done

  let add_clauses ?tag cnf =
    let aux cl =
      let c = make_clause ?tag (fresh_hname ()) cl (List.length cl) false (History []) (current_level ()) in
      add_clause c;
      match propagate () with
      | None -> () | Some confl -> report_unsat confl
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
            let tag = ref false in
            Th.if_sat (full_slice tag);
            if not !tag then raise Sat
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
    St.iter_elt (fun e -> destruct_elt e
                     (fun v -> L.debug 50 " -- %a" St.pp_lit v)
                     (fun a -> L.debug 50 " -- %a" St.pp_atom a.pa)
                 );
    add_clauses ?tag cnf

  let assume ?tag cnf =
    let cnf = List.rev_map (List.rev_map atom) cnf in
    init_solver ?tag cnf

  let eval lit =
    let var, negated = make_boolean_var lit in
    assert (var.pa.is_true || var.na.is_true);
    let truth = var.pa.is_true in
    if negated then not truth else truth

  let hyps () = env.clauses_hyps

  let history () = env.clauses_learnt

  let unsat_conflict () = env.unsat_conflict

  let model () =
    let opt = function Some a -> a | None -> assert false in
    Vec.fold (fun acc e -> destruct e
                 (fun v -> (v.term, opt v.assigned)  :: acc)
                 (fun _ -> acc)
             ) [] env.elt_queue

  (* Push/Pop *)
  let push () =
    if is_unsat () then current_level ()
    else begin
      let res = current_level () in
      let ul_trail =
        if Vec.is_empty env.elt_levels then Vec.size env.elt_queue
        else Vec.get env.elt_levels 0
      and ul_th_env =
        if Vec.is_empty env.th_levels then Th.current_level ()
        else Vec.get env.th_levels 0
      in
      let ul_clauses = Vec.size env.clauses_hyps in
      let ul_learnt = Vec.size env.clauses_learnt in
      let ul_proof_lvl = Proof.push () in
      Vec.push env.user_levels {ul_trail; ul_th_env; ul_clauses; ul_learnt; ul_proof_lvl;};
      res
    end

  (* Backtrack to decision_level 0, with trail_lim && theory env specified *)
  let reset_until push_lvl trail_lim th_env =
    L.debug 1 "Resetting to decision level 0 (pop/forced)";
    env.elt_head <- trail_lim;
    env.th_head <- env.elt_head;
    for c = env.elt_head to Vec.size env.elt_queue - 1 do
      destruct (Vec.get env.elt_queue c)
        (fun v ->
           v.assigned <- None;
           v.level <- -1;
           insert_var_order (elt_of_lit v)
        )
        (fun a ->
           match a.var.reason with
           | Bcp Some { c_level } when c_level > push_lvl ->
             a.is_true <- false;
             a.neg.is_true <- false;
             a.var.level <- -1;
             a.var.reason <- Bcp None;
             insert_var_order (elt_of_var a.var)
           | _ ->
             Vec.set env.elt_queue env.elt_head (of_atom a);
             env.elt_head <- env.elt_head + 1
        )
    done;
    Th.backtrack th_env; (* recover the right tenv *)
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
      reset_until l ul.ul_trail ul.ul_th_env;

      (* Log current assumptions for debugging purposes *)
      for i = 0 to Vec.size env.elt_queue - 1 do
        L.debug 99 "  %d -- %a" i (fun fmt e ->
            destruct e (St.pp_lit fmt) (St.pp_atom fmt)) (Vec.get env.elt_queue i)
      done;

      (* Clear hypothesis not valid anymore *)
      for i = ul.ul_clauses to Vec.size env.clauses_hyps - 1 do
        let c = Vec.get env.clauses_hyps i in
        assert (c.c_level > l);
        remove_clause c
      done;
      Vec.shrink env.clauses_hyps (Vec.size env.clauses_hyps - ul.ul_clauses);

      (* Backtrack the Proof module *)
      Proof.pop ul.ul_proof_lvl;

      (* Refresh the known tautologies simplified because of clauses that have been removed *)
      let s = Stack.create () in
      let new_sz = ref ul.ul_learnt in
      for i = ul.ul_learnt to Vec.size env.clauses_learnt - 1 do
        let c = Vec.get env.clauses_learnt i in
        if c.c_level > l then begin
          remove_clause c;
          match c.cpremise with
          | History [ { cpremise = Lemma _ } as c' ] -> Stack.push c' s
          | _ -> () (* Only simplified clauses can have a level > 0 *)
        end else begin
          L.debug 15 "Keeping intact clause %a" St.pp_clause c;
          Vec.set env.clauses_learnt !new_sz c;
          incr new_sz
        end
      done;
      Vec.shrink env.clauses_learnt (Vec.size env.clauses_learnt - !new_sz);
      Stack.iter (add_clause ~force:true) s
    end

  let reset () = pop base_level
end

