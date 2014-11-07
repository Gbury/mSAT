(**************************************************************************)
(*                                                                        *)
(*                          Alt-Ergo Zero                                 *)
(*                                                                        *)
(*                  Sylvain Conchon and Alain Mebsout                     *)
(*                      Universite Paris-Sud 11                           *)
(*                                                                        *)
(*  Copyright 2011. This file is distributed under the terms of the       *)
(*  Apache Software License version 2.0                                   *)
(*                                                                        *)
(**************************************************************************)

module Make (F : Formula_intf.S)
    (St : Solver_types.S with type formula = F.t)
    (Ex : Explanation.S with type atom = St.atom)
    (Th : Theory_intf.S with type formula = F.t and type explanation = Ex.t) = struct

  open St
  module Proof = Res.Make(St)(Th)

  exception Sat
  exception Unsat
  exception Restart
  exception Conflict of clause

  (* Singleton type containing the current state *)
  type env = {

    mutable is_unsat : bool;
    (* if [true], constraints are already false *)

    mutable unsat_conflict : clause option;
    (* conflict clause at decision level 0, if any *)

    clauses : clause Vec.t;
    (* all currently active clauses *)

    learnts : clause Vec.t;
    (* learnt clauses  *)

    mutable clause_inc : float;
    (* increment for clauses' activity *)

    mutable var_inc : float;
    (* increment for variables' activity *)

    vars : var Vec.t;
    (* all boolean variables *)

    trail : atom Vec.t;
    (* decision stack + propagated atoms *)

    trail_lim : int Vec.t;
    (* decision levels in [trail]  *)

    levels : int Vec.t;
    (* user-defined levels. Subset of [trail_lim] *)

    mutable qhead : int;
    (* Start offset in the queue of unit facts to propagate, within the trail *)

    mutable simpDB_assigns : int;
    (* number of toplevel assignments since last call to [simplify ()] *)

    mutable simpDB_props : int;
    (* remaining number of propagations before the next call to [simplify ()] *)

    order : Iheap.t;
    (* Heap ordered by variable activity *)

    mutable progress_estimate : float;
    (* progression estimate, updated by [search ()] *)

    remove_satisfied : bool;

    var_decay : float;
    (* inverse of the activity factor for variables. Default 1/0.999 *)

    clause_decay : float;
    (* inverse of the activity factor for clauses. Default 1/0.95 *)

    mutable restart_first : int;
    (* intial restart limit, default 100 *)

    restart_inc : float;
    (* multiplicative factor for restart limit, default 1.5 *)

    mutable learntsize_factor : float;
    (* initial limit for the number of learnt clauses, 1/3 of initial
        number of clauses by default *)

    learntsize_inc : float;
    (* multiplicative factor for [learntsize_factor] at each restart, default 1.1 *)

    expensive_ccmin : bool;
    (* control minimization of conflict clause, default true *)

    polarity_mode : bool;
    (* default polarity for decision *)

    mutable starts : int;
    mutable decisions : int;
    mutable propagations : int;
    mutable conflicts : int;
    mutable clauses_literals : int;
    mutable learnts_literals : int;
    mutable max_literals : int;
    mutable tot_literals : int;
    mutable nb_init_vars : int;
    mutable nb_init_clauses : int;
    mutable model : var Vec.t;
    mutable tenv : Th.t;
    mutable tenv_queue : Th.t Vec.t;
    mutable tatoms_queue : atom Queue.t;
  }

  let env = {
    is_unsat = false;
    unsat_conflict = None;
    clauses = Vec.make 0 dummy_clause; (*updated during parsing*)
    learnts = Vec.make 0 dummy_clause; (*updated during parsing*)
    clause_inc = 1.;
    var_inc = 1.;
    vars = Vec.make 0 dummy_var; (*updated during parsing*)
    trail = Vec.make 601 dummy_atom;
    trail_lim = Vec.make 601 (-1);
    levels =  Vec.make 20 (-1);
    qhead = 0;
    simpDB_assigns = -1;
    simpDB_props = 0;
    order = Iheap.init 0; (* updated in solve *)
    progress_estimate = 0.;
    remove_satisfied = true;
    var_decay = 1. /. 0.95;
    clause_decay = 1. /. 0.999;
    restart_first = 100;
    restart_inc = 1.5;
    learntsize_factor = 1. /. 3. ;
    learntsize_inc = 1.1;
    expensive_ccmin = true;
    polarity_mode = false;
    starts = 0;
    decisions = 0;
    propagations = 0;
    conflicts = 0;
    clauses_literals = 0;
    learnts_literals = 0;
    max_literals = 0;
    tot_literals = 0;
    nb_init_vars = 0;
    nb_init_clauses = 0;
    model = Vec.make 0 dummy_var;
    tenv = Th.empty();
    tenv_queue = Vec.make 100 Th.dummy;
    tatoms_queue = Queue.create ();
  }

  let f_weight i j =
    (Vec.get env.vars j).weight < (Vec.get env.vars i).weight

  let f_filter i =
    (Vec.get env.vars i).level < 0

  let insert_var_order v =
    Iheap.insert f_weight env.order v.vid

  let var_decay_activity () =
    env.var_inc <- env.var_inc *. env.var_decay

  let clause_decay_activity () =
    env.clause_inc <- env.clause_inc *. env.clause_decay

  let var_bump_activity v =
    v.weight <- v.weight +. env.var_inc;
    if v.weight > 1e100 then begin
      for i = 0 to (Vec.size env.vars) - 1 do
        (Vec.get env.vars i).weight <- (Vec.get env.vars i).weight *. 1e-100
      done;
      env.var_inc <- env.var_inc *. 1e-100;
    end;
    if Iheap.in_heap env.order v.vid then
      Iheap.decrease f_weight env.order v.vid


  let clause_bump_activity c =
    c.activity <- c.activity +. env.clause_inc;
    if c.activity > 1e20 then begin
      for i = 0 to (Vec.size env.learnts) - 1 do
        (Vec.get env.learnts i).activity <-
          (Vec.get env.learnts i).activity *. 1e-20;
      done;
      env.clause_inc <- env.clause_inc *. 1e-20
    end

  let decision_level () = Vec.size env.trail_lim

  let nb_assigns () = Vec.size env.trail
  let nb_clauses () = Vec.size env.clauses
  let nb_learnts () = Vec.size env.learnts
  let nb_vars    () = Vec.size env.vars

  let new_decision_level() =
    Vec.push env.trail_lim (Vec.size env.trail);
    Vec.push env.tenv_queue env.tenv; (* save the current tenv *)
    Log.debug 5 "New decision level : %d (%d in env queue)(%d in trail)"
      (Vec.size env.trail_lim) (Vec.size env.tenv_queue) (Vec.size env.trail);
    ()

  let attach_clause c =
    Vec.push (Vec.get c.atoms 0).neg.watched c;
    Vec.push (Vec.get c.atoms 1).neg.watched c;
    Log.debug 8 "%a <-- %a" St.pp_atom (Vec.get c.atoms 0).neg St.pp_clause c;
    Log.debug 8 "%a <-- %a" St.pp_atom (Vec.get c.atoms 1).neg St.pp_clause c;
    if c.learnt then
      env.learnts_literals <- env.learnts_literals + Vec.size c.atoms
    else
      env.clauses_literals <- env.clauses_literals + Vec.size c.atoms

  let detach_clause c =
    c.removed <- true;
        (*
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
    Log.debug 5 "Bactracking to decision level %d (excluded)" lvl;
    if decision_level () > lvl then begin
      env.qhead <- Vec.get env.trail_lim lvl;
      for c = Vec.size env.trail - 1 downto env.qhead do
        let a = Vec.get env.trail c in
        a.is_true <- false;
        a.neg.is_true <- false;
        a.var.level <- -1;
        a.var.reason <- None;
        a.var.vpremise <- [];
        insert_var_order a.var
      done;
      Queue.clear env.tatoms_queue;
      env.tenv <- Vec.get env.tenv_queue lvl; (* recover the right tenv *)
      Vec.shrink env.trail ((Vec.size env.trail) - env.qhead);
      Vec.shrink env.trail_lim ((Vec.size env.trail_lim) - lvl);
      Vec.shrink env.tenv_queue ((Vec.size env.tenv_queue) - lvl)
    end;
    assert (Vec.size env.trail_lim = Vec.size env.tenv_queue)

  let rec pick_branch_lit () =
    let max = Iheap.remove_min f_weight env.order in
    let v = Vec.get env.vars max in
    if v.level>= 0 then  begin
      assert (v.pa.is_true || v.na.is_true);
      pick_branch_lit ()
    end else
      v

  let enqueue a lvl reason =
    assert (not a.is_true && not a.neg.is_true &&
            a.var.level < 0 && a.var.reason = None && lvl >= 0);
    (* keep the reason for proof/unsat-core *)
    (*let reason = if lvl = 0 then None else reason in*)
    a.is_true <- true;
    a.var.level <- lvl;
    a.var.reason <- reason;
    Log.debug 8 "Enqueue: %a" pp_atom a;
    Vec.push env.trail a

  let progress_estimate () =
    let prg = ref 0. in
    let nbv = to_float (nb_vars()) in
    let lvl = decision_level () in
    let _F = 1. /. nbv in
    for i = 0 to lvl do
      let _beg = if i = 0 then 0 else Vec.get env.trail_lim (i-1) in
      let _end = if i=lvl then Vec.size env.trail else Vec.get env.trail_lim i in
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
            Log.debug 8 "New watcher (%a) for clause : %a" St.pp_atom ak.neg St.pp_clause c;
            raise Exit
          end
        done;
        (* no watch lit found *)
        if first.neg.is_true then begin
          (* clause is false *)
          env.qhead <- Vec.size env.trail;
          for k = i to Vec.size watched - 1 do
            Vec.set watched !new_sz (Vec.get watched k);
            incr new_sz;
          done;
          Log.debug 3 "Conflict found : %a" St.pp_clause c;
          raise (Conflict c)
        end
        else begin
          (* clause is unit *)
          Vec.set watched !new_sz c;
          incr new_sz;
          Log.debug 5 "Unit clause : %a" St.pp_clause c;
          enqueue first (decision_level ()) (Some c)
        end
      with Exit -> ()

  let propagate_atom a res =
    Log.debug 8 "Propagating %a" St.pp_atom a;
    let watched = a.watched in
    Log.debug 10 "Watching %a :" St.pp_atom a;
    Vec.iter (fun c -> Log.debug 10 "  %a" St.pp_clause c) watched;
    let new_sz_w = ref 0 in
    begin
      try
        for i = 0 to Vec.size watched - 1 do
          let c = Vec.get watched i in
          if not c.removed then propagate_in_clause a c i watched new_sz_w
        done;
      with Conflict c -> assert (!res = None); res := Some c
    end;
    let dead_part = Vec.size watched - !new_sz_w in
    Vec.shrink watched dead_part

  let expensive_theory_propagate () = None
  (* try *)
  (*   if D1.d then eprintf "expensive_theory_propagate@."; *)
  (*   ignore(Th.expensive_processing env.tenv); *)
  (*   if D1.d then eprintf "expensive_theory_propagate => None@."; *)
  (*   None *)
  (* with Th.Inconsistent dep ->  *)
  (*   if D1.d then eprintf "expensive_theory_propagate => Inconsistent@."; *)
  (*   Some dep *)

  let theory_propagate () =
    let facts = ref [] in
    while not (Queue.is_empty env.tatoms_queue) do
      let a = Queue.pop env.tatoms_queue in
      if a.is_true then
        (*let ex = if a.var.level > 0 then Ex.singleton a else Ex.empty in*)
        let ex = Ex.singleton a in (* Usefull for debugging *)
        facts := (a.lit, ex) :: !facts
      else
      if a.neg.is_true then
        (*let ex = if a.var.level > 0 then Ex.singleton a.neg else Ex.empty in*)
        let ex = Ex.singleton a.neg in (* Usefull for debugging *)
        facts := (a.neg.lit, ex) :: !facts
      else assert false;
    done;
    try
      let full_model = nb_assigns() = env.nb_init_vars in
      env.tenv <-
        List.fold_left
          (fun t (a,ex) -> let t = Th.assume ~cs:true a ex t in t)
          env.tenv !facts;
      if full_model then expensive_theory_propagate ()
      else None
    with Th.Inconsistent dep ->
      (* eprintf "th inconsistent : %a @." Ex.print dep; *)
      Some dep

  (* boolean propagation, using unit clauses *)
  let propagate () =
    let num_props = ref 0 in
    let res = ref None in
    (*assert (Queue.is_empty env.tqueue);*)
    while env.qhead < Vec.size env.trail do
      let a = Vec.get env.trail env.qhead in
      env.qhead <- env.qhead + 1;
      incr num_props;
      propagate_atom a res;
      Queue.push a env.tatoms_queue;
    done;
    env.propagations <- env.propagations + !num_props;
    env.simpDB_props <- env.simpDB_props - !num_props;
    !res

  (* conflict analysis *)
  let analyze c_clause =
    let pathC  = ref 0 in
    let learnt = ref [] in
    let cond   = ref true in
    let blevel = ref 0 in
    let seen   = ref [] in
    let c      = ref c_clause in
    let tr_ind = ref (Vec.size env.trail - 1) in
    let size   = ref 1 in
    let history = ref [] in
    while !cond do
      if !c.learnt then clause_bump_activity !c;
      history := !c :: !history;
      (* visit the current predecessors *)
      for j = 0 to Vec.size !c.atoms - 1 do
        let q = Vec.get !c.atoms j in
        (*printf "I visit %a@." D1.atom q;*)
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
      while not (Vec.get env.trail !tr_ind).var.seen do decr tr_ind done;
      decr pathC;
      let p = Vec.get env.trail !tr_ind in
      decr tr_ind;
      match !pathC, p.var.reason with
      | 0, _ ->
        cond := false;
        learnt := p.neg :: (List.rev !learnt)
      | n, None   -> assert false
      | n, Some cl -> c := cl
    done;
    List.iter (fun q -> q.var.seen <- false) !seen;
    !blevel, !learnt, !history, !size

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


  let report_b_unsat ({atoms=atoms} as confl) =
    env.unsat_conflict <- Some confl;
    env.is_unsat <- true;
    raise Unsat

  let report_t_unsat dep =
    env.is_unsat <- true;
    raise Unsat

  let simplify () =
    assert (decision_level () = 0);
    if env.is_unsat then raise Unsat;
    begin
      match propagate () with
      | Some confl -> report_b_unsat confl
      | None ->
        match theory_propagate () with
          Some dep -> report_t_unsat dep
        | None -> ()
    end;
    if nb_assigns() <> env.simpDB_assigns && env.simpDB_props <= 0 then begin
      if Vec.size env.learnts > 0 then remove_satisfied env.learnts;
      if env.remove_satisfied then remove_satisfied env.clauses;
      (*Iheap.filter env.order f_filter f_weight;*)
      env.simpDB_assigns <- nb_assigns ();
      env.simpDB_props <- env.clauses_literals + env.learnts_literals;
    end

  let record_learnt_clause blevel learnt history size =
    begin match learnt with
      | [] -> assert false
      | [fuip] ->
        assert (blevel = 0);
        fuip.var.vpremise <- history;
        let name = fresh_lname () in
        let uclause = make_clause name learnt size true history in
        Log.debug 2 "Unit clause learnt : %a" St.pp_clause uclause;
        Vec.push env.learnts uclause;
        enqueue fuip 0 (Some uclause)
      | fuip :: _ ->
        let name = fresh_lname () in
        let lclause = make_clause name learnt size true history in
        Log.debug 2 "New clause learnt : %a" St.pp_clause lclause;
        Vec.push env.learnts lclause;
        attach_clause lclause;
        clause_bump_activity lclause;
        enqueue fuip blevel (Some lclause)
    end;
    var_decay_activity ();
    clause_decay_activity ()

  let check_inconsistency_of dep =
    try
      let env = ref (Th.empty()) in ();
      Ex.iter_atoms
        (fun atom ->
           let t = Th.assume ~cs:true atom.lit (Ex.singleton atom) !env in
           env := t)
        dep;
      (* ignore (Th.expensive_processing !env); *)
      assert false
    with Th.Inconsistent _ -> ()

  let theory_analyze dep =
    let atoms, sz, max_lvl, c_hist =
      Ex.fold_atoms
        (fun a (acc, sz, max_lvl, c_hist) ->
           let c_hist = List.rev_append a.var.vpremise c_hist in
           let c_hist = match a.var.reason with
             | None -> c_hist | Some r -> r:: c_hist in
           if a.var.level = 0 then acc, sz, max_lvl, c_hist
           else a.neg :: acc, sz + 1, max max_lvl a.var.level, c_hist
        ) dep ([], 0, 0, [])
    in
    if atoms = [] then begin
      (* check_inconsistency_of dep; *)
      report_t_unsat dep
      (* une conjonction de faits unitaires etaient deja unsat *)
    end;
    let name = fresh_dname() in
    let c_clause = make_clause name atoms sz false c_hist in
    (* eprintf "c_clause: %a@." Debug.clause c_clause; *)
    c_clause.removed <- true;

    let pathC  = ref 0 in
    let learnt = ref [] in
    let cond   = ref true in
    let blevel = ref 0 in
    let seen   = ref [] in
    let c      = ref c_clause in
    let tr_ind = ref (Vec.size env.trail - 1) in
    let size   = ref 1 in
    let history = ref [] in
    while !cond do
      if !c.learnt then clause_bump_activity !c;
      history := !c :: !history;
      (* visit the current predecessors *)
      for j = 0 to Vec.size !c.atoms - 1 do
        let q = Vec.get !c.atoms j in
        (*printf "I visit %a@." D1.atom q;*)
        assert (q.is_true || q.neg.is_true && q.var.level >= 0); (* Pas sur *)
        if not q.var.seen && q.var.level > 0 then begin
          var_bump_activity q.var;
          q.var.seen <- true;
          seen := q :: !seen;
          if q.var.level >= max_lvl then incr pathC
          else begin
            learnt := q :: !learnt;
            incr size;
            blevel := max !blevel q.var.level
          end
        end
      done;

      (* look for the next node to expand *)
      while not (Vec.get env.trail !tr_ind).var.seen do decr tr_ind done;
      decr pathC;
      let p = Vec.get env.trail !tr_ind in
      decr tr_ind;
      match !pathC, p.var.reason with
      | 0, _ ->
        cond := false;
        learnt := p.neg :: (List.rev !learnt)
      | n, None   -> assert false
      | n, Some cl -> c := cl
    done;
    List.iter (fun q -> q.var.seen <- false) !seen;
    !blevel, !learnt, !history, !size

  let add_boolean_conflict confl =
    env.conflicts <- env.conflicts + 1;
    if decision_level() = 0 then report_b_unsat confl; (* Top-level conflict *)
    let blevel, learnt, history, size = analyze confl in
    cancel_until blevel;
    record_learnt_clause blevel learnt history size

  let search n_of_conflicts n_of_learnts =
    let conflictC = ref 0 in
    env.starts <- env.starts + 1;
    while (true) do
      match propagate () with
      | Some confl -> (* Conflict *)
        incr conflictC;
        add_boolean_conflict confl

      | None -> (* No Conflict *)
        match theory_propagate () with
        | Some dep ->
          incr conflictC;
          env.conflicts <- env.conflicts + 1;
          if decision_level() = 0 then report_t_unsat dep; (* T-L conflict *)
          let blevel, learnt, history, size = theory_analyze dep in
          cancel_until blevel;
          record_learnt_clause blevel learnt history size

        | None ->
          if nb_assigns() = env.nb_init_vars then raise Sat;
          if n_of_conflicts >= 0 && !conflictC >= n_of_conflicts then
            begin
              env.progress_estimate <- progress_estimate();
              cancel_until 0;
              raise Restart
            end;
          if decision_level() = 0 then simplify ();

          if n_of_learnts >= 0 &&
             Vec.size env.learnts - nb_assigns() >= n_of_learnts then
            reduce_db();

          env.decisions <- env.decisions + 1;

          new_decision_level();
          let next = pick_branch_lit () in
          let current_level = decision_level () in
          assert (next.level < 0);
          Log.debug 5 "Deciding on %a" St.pp_atom next.pa;
          enqueue next.pa current_level None
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

  let check_model () =
    check_vec env.clauses;
    check_vec env.learnts

  (* fixpoint of propagation and decisions until a model is found, or a
     conflict is reached *)
  let solve () =
    if env.is_unsat then raise Unsat;
    let n_of_conflicts = ref (to_float env.restart_first) in
    let n_of_learnts = ref ((to_float (nb_clauses())) *. env.learntsize_factor) in
    try
      while true do
        begin try
            search (to_int !n_of_conflicts) (to_int !n_of_learnts);
          with Restart -> ()
        end;
        n_of_conflicts := !n_of_conflicts *. env.restart_inc;
        n_of_learnts   := !n_of_learnts *. env.learntsize_inc;
      done;
    with
    | Sat -> ()

  exception Trivial

  (* TODO: could be more efficient than [@] everywhere? *)
  let partition atoms init =
    let rec partition_aux trues unassigned falses init = function
      | [] -> trues @ unassigned @ falses, init
      | a::r ->
        if a.is_true then
          if a.var.level = 0 then raise Trivial
          else (a::trues) @ unassigned @ falses @ r, init
        else if a.neg.is_true then
          if a.var.level = 0 then
            partition_aux trues unassigned falses
              (List.rev_append (a.var.vpremise) init) r
          else partition_aux trues unassigned (a::falses) init r
        else partition_aux trues (a::unassigned) falses init r
    in
    partition_aux [] [] [] init atoms


  let add_clause ~cnumber atoms =
    if env.is_unsat then raise Unsat;
    let init_name = string_of_int cnumber in
    let init0 = make_clause init_name atoms (List.length atoms) false [] in
    try
      let atoms, init =
        if decision_level () = 0 then
          let atoms, init = List.fold_left
              (fun (atoms, init) a ->
                 if a.is_true then raise Trivial;
                 if a.neg.is_true then
                   atoms, (List.rev_append (a.var.vpremise) init)
                 else a::atoms, init
              ) ([], [init0]) atoms in
          List.fast_sort (fun a b -> a.var.vid - b.var.vid) atoms, init
        else partition atoms [init0]
      in
      let size = List.length atoms in
      match atoms with
      | [] ->
        report_b_unsat init0;

      | a::_::_ ->
        let name = fresh_name () in
        let clause = make_clause name atoms size false init in
        attach_clause clause;
        Vec.push env.clauses clause;
        if a.neg.is_true then begin
          let lvl = List.fold_left (fun m a -> max m a.var.level) 0 atoms in
          cancel_until lvl;
          add_boolean_conflict clause
        end

      | [a]   ->
        cancel_until 0;
        a.var.vpremise <- init;
        enqueue a 0 None;
        match propagate () with
          None -> () | Some confl -> report_b_unsat confl
    with Trivial -> ()

  let add_clauses cnf ~cnumber =
    List.iter (add_clause ~cnumber) cnf;
    match theory_propagate () with
      None -> () | Some dep -> report_t_unsat dep

  let init_solver cnf ~cnumber =
    let nbv = made_vars_info env.vars in
    let nbc = env.nb_init_clauses + List.length cnf in
    Vec.grow_to_by_double env.vars nbv;
    Iheap.grow_to_by_double env.order nbv;
    List.iter
      (List.iter
         (fun a ->
            insert_var_order a.var
         )
      ) cnf;
    env.nb_init_vars <- nbv;
    Vec.grow_to_by_double env.model nbv;
    Vec.grow_to_by_double env.clauses nbc;
    Vec.grow_to_by_double env.learnts nbc;
    env.nb_init_clauses <- nbc;
    add_clauses cnf ~cnumber


  let assume cnf ~cnumber =
    let cnf = List.rev_map (List.rev_map St.add_atom) cnf in
    init_solver cnf ~cnumber

  let eval lit =
    let var, negated = make_var lit in
    let truth = var.pa.is_true in
    if negated then not truth else truth

  let history () = env.learnts

  let unsat_conflict () = env.unsat_conflict

  type level = int

  let base_level = 0

  let current_level() =
    if Vec.is_empty env.levels then base_level else Vec.last env.levels

  let push () =
    let l = if Vec.is_empty env.trail_lim
      then base_level
      else Vec.last env.trail_lim
    in
    Vec.push env.levels l;
    l

  let pop l =
    if l > current_level()
    then invalid_arg "cannot pop() to level, it is too high";
    let i = Vec.get env.levels l in
    (* see whether we can reset [env.is_unsat] *)
    if env.is_unsat && not (Vec.is_empty env.trail_lim) then (
      (* level at which the decision that lead to unsat was made *)
      let last = Vec.last env.trail_lim in
      if i < last then env.is_unsat <- false
    );
    cancel_until i

  let clear () = pop base_level
end

