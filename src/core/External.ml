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

type 'clause export = 'clause Solver_intf.export = {
  hyps: 'clause Vec.t;
  history: 'clause Vec.t;
  local: 'clause Vec.t;
}

module Make
    (St : Solver_types.S)
    (Th : Plugin_intf.S with type term = St.term
                         and type formula = St.formula
                         and type proof = St.proof)
    ()
= struct

  module St = St

  module S = Internal.Make(St)(Th)(struct end)

  module Proof = S.Proof

  exception UndecidedLit = S.UndecidedLit

  type atom = St.formula

  (* Result type *)
  type res =
    | Sat of (St.term,St.formula) sat_state
    | Unsat of (St.clause,Proof.proof) unsat_state

  let pp_all lvl status =
    Log.debugf lvl
      (fun k -> k
        "@[<v>%s - Full resume:@,@[<hov 2>Trail:@\n%a@]@,@[<hov 2>Temp:@\n%a@]@,@[<hov 2>Hyps:@\n%a@]@,@[<hov 2>Lemmas:@\n%a@]@,@]@."
          status
          (Vec.print ~sep:"" St.pp) (S.trail ())
          (Vec.print ~sep:"" St.pp_clause) (S.temp ())
          (Vec.print ~sep:"" St.pp_clause) (S.hyps ())
          (Vec.print ~sep:"" St.pp_clause) (S.history ())
      )

  let mk_sat () : (_,_) sat_state =
    pp_all 99 "SAT";
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
    pp_all 99 "UNSAT";
    let unsat_conflict () =
      match S.unsat_conflict () with
      | None -> assert false
      | Some c -> c
    in
    let get_proof () =
      let c = unsat_conflict () in
      S.Proof.prove_unsat c
    in
    { unsat_conflict; get_proof; }

  (* Wrappers around internal functions*)
  let assume = S.assume

  let solve ?(assumptions=[]) () =
    try
      S.pop (); (* FIXME: what?! *)
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

  let new_lit = S.new_lit
  let new_atom = S.new_atom

  let export () : St.clause export =
    let hyps = S.hyps () in
    let history = S.history () in
    let local = S.temp () in
    {hyps; history; local}
end
