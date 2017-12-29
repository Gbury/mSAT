(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2016 Guillaume Bury
Copyright 2016 Simon Cruanes
*)

module type S = Solver_intf.S

open Solver_intf

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
          (Vec.print ~sep:"" St.Trail_elt.debug) (S.trail ())
          (Vec.print ~sep:"" St.Clause.debug) (S.temp ())
          (Vec.print ~sep:"" St.Clause.debug) (S.hyps ())
          (Vec.print ~sep:"" St.Clause.debug) (S.history ())
      )

  let mk_sat () : (_,_) sat_state =
    pp_all 99 "SAT";
    let t = S.trail () in
    let iter f f' =
      Vec.iter (function
          | St.Atom a -> f a.St.lit
          | St.Lit l -> f' l.St.term)
        t
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
