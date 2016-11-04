(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2016 Guillaume Bury
Copyright 2016 Simon Cruanes
*)

module type S = Solver_intf.S

type ('term, 'form) sat_state = ('term, 'form) Solver_intf.sat_state = {
  eval: 'form -> bool;
  (** Returns the valuation of a formula in the current state
      of the sat solver.
      @raise UndecidedLit if the literal is not decided *)
  eval_level: 'form -> bool * int;
  (** Return the current assignement of the literals, as well as its
      decision level. If the level is 0, then it is necessary for
      the atom to have this value; otherwise it is due to choices
      that can potentially be backtracked.
      @raise UndecidedLit if the literal is not decided *)
  iter_trail : ('form -> unit) -> ('term -> unit) -> unit;
  (** Iter thorugh the formulas and terms in order of decision/propagation
      (starting from the first propagation, to the last propagation). *)
  model: unit -> ('term * 'term) list;
  (** Returns the model found if the formula is satisfiable. *)
}

type ('clause, 'proof) unsat_state = ('clause, 'proof) Solver_intf.unsat_state = {
  unsat_conflict : unit -> 'clause;
  (** Returns the unsat clause found at the toplevel *)
  get_proof : unit -> 'proof;
  (** returns a persistent proof of the empty clause from the Unsat result. *)
}


module Make
    (St : Solver_types.S)
    (Th : Plugin_intf.S with type term = St.term
                         and type formula = St.formula
                         and type proof = St.proof)
    (Dummy : sig end) = struct

  module St = St

  module S = Internal.Make(St)(Th)(struct end)

  module Proof = S.Proof

  exception UndecidedLit = S.UndecidedLit

  type atom = St.formula
  type clause = St.clause
  type proof = Proof.proof

  (* Result type *)
  type res =
    | Sat of (St.term,St.formula) sat_state
    | Unsat of (St.clause,Proof.proof) unsat_state

  let pp_all lvl =
    Log.debugf lvl
      "@[<v>Full resume:@,@[<hov 2>Trail:@\n%a@]@,@[<hov 2>Temp:@\n%a@]@,@[<hov 2>Hyps:@\n%a@]@,@[<hov 2>Lemmas:@\n%a@]@,]@."
      (fun k -> k
          (Vec.print ~sep:"" St.pp) (S.trail ())
          (Vec.print ~sep:"" St.pp_clause) (S.temp ())
          (Vec.print ~sep:"" St.pp_clause) (S.hyps ())
          (Vec.print ~sep:"" St.pp_clause) (S.history ())
      )

  let mk_sat () : (_,_) sat_state =
    pp_all 99;
    let t = S.trail () in
    let iter f f' =
      Vec.iter (function
          | St.Atom a -> f a.St.lit
          | St.Lit l -> f' l.St.term
        ) t
    in
    {
      eval = S.eval;
      eval_level = S.eval_level;
      iter_trail = iter;
      model = S.model;
    }

  let mk_unsat () : (_,_) unsat_state =
    pp_all 99;
    let unsat_conflict () = match S.unsat_conflict () with
      | None -> assert false
      | Some c -> c
    in
    let get_proof () =
      let c = unsat_conflict() in
      S.Proof.prove_unsat c
    in
    { unsat_conflict; get_proof; }

  (* Wrappers around internal functions*)
  let assume = S.assume

  let solve ?(assumptions=[]) () =
    try
      S.pop ();
      S.push ();
      S.local assumptions;
      S.solve ();
      Sat (mk_sat())
    with S.Unsat ->
      Unsat (mk_unsat())

  let unsat_core = S.Proof.unsat_core

  let true_at_level0 a =
    try
      let b, lev = S.eval_level a in
      b && lev = 0
    with S.UndecidedLit -> false

  let get_tag cl = St.(cl.tag)

  let export_dimacs fmt () =
    (* Local assertions *)
    let tmp = S.temp () in
    assert (Vec.for_all (function
        | { St.cpremise = St.Local; _} -> true | _ -> false
      ) tmp);
    (* Problem hypothses *)
    let hyps = S.hyps () in
    assert (Vec.for_all (function
        | { St.cpremise = St.Hyp; _} -> true | _ -> false
      ) hyps);
    (* Learnt clauses, then filtered to only keep only
       the theory lemmas; all other learnt clauses should be logical
       consequences of the rest. *)
    let learnt = S.history () in
    let lemmas = Vec.make (Vec.size learnt) St.dummy_clause in
    Vec.iter (fun c ->
        match c.St.cpremise with
        | St.Hyp | St.Local -> assert false
        | St.Lemma _ -> Vec.push lemmas c
        | St.History l ->
          begin match l with
            | [] -> assert false
            | d :: _ ->
              begin match d.St.cpremise with
                | St.Lemma _ -> Vec.push lemmas d
                | St.Hyp | St.Local | St.History _ -> ()
              end
          end
      ) learnt;
    (* Number of atoms and clauses *)
    let n = St.nb_elt () in
    let m = Vec.size tmp + Vec.size hyps + Vec.size lemmas in
    (* Actual printing *)
    let pp s fmt vec =
      Format.fprintf fmt "c %s@,%a@," s (Vec.print ~sep:"" St.pp_dimacs) vec
    in
    Format.fprintf fmt
      "@[<v>p cnf %d %d@,%a%a%a@]@."
      n m
      (pp "Local assumptions") tmp
      (pp "Hypotheses") hyps
      (pp "Lemmas") lemmas

end
