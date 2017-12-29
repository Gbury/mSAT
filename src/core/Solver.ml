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
= struct

  module St = St

  module S = Internal.Make(St)(Th)

  module Proof = S.Proof

  exception UndecidedLit = S.UndecidedLit

  type formula = St.formula
  type term = St.term
  type atom = St.formula
  type clause = St.clause

  type t = S.t
  type solver = t

  let[@inline] create ?size () = S.create ?size ()

  (* Result type *)
  type res =
    | Sat of (St.term,St.formula) sat_state
    | Unsat of (St.clause,Proof.proof) unsat_state

  let pp_all st lvl status =
    Log.debugf lvl
      (fun k -> k
          "@[<v>%s - Full resume:@,@[<hov 2>Trail:@\n%a@]@,\
           @[<hov 2>Temp:@\n%a@]@,@[<hov 2>Hyps:@\n%a@]@,@[<hov 2>Lemmas:@\n%a@]@,@]@."
          status
          (Vec.print ~sep:"" St.Trail_elt.debug) (S.trail st)
          (Vec.print ~sep:"" St.Clause.debug) (S.temp st)
          (Vec.print ~sep:"" St.Clause.debug) (S.hyps st)
          (Vec.print ~sep:"" St.Clause.debug) (S.history st)
      )

  let mk_sat (st:S.t) : (_,_) sat_state =
    pp_all st 99 "SAT";
    let t = S.trail st in
    let iter f f' =
      Vec.iter (function
          | St.Atom a -> f a.St.lit
          | St.Lit l -> f' l.St.term)
        t
    in
    {
      eval = S.eval st;
      eval_level = S.eval_level st;
      iter_trail = iter;
      model = (fun () -> S.model st);
    }

  let mk_unsat (st:S.t) : (_,_) unsat_state =
    pp_all st 99 "UNSAT";
    let unsat_conflict () =
      match S.unsat_conflict st with
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

  let add_clause = S.add_clause

  let solve (st:t) ?(assumptions=[]) () =
    try
      S.pop st; (* FIXME: what?! *)
      S.push st;
      S.local st assumptions;
      S.solve st;
      Sat (mk_sat st)
    with S.Unsat ->
      Unsat (mk_unsat st)

  let unsat_core = S.Proof.unsat_core

  let true_at_level0 st a =
    try
      let b, lev = S.eval_level st a in
      b && lev = 0
    with S.UndecidedLit -> false

  let get_tag cl = St.(cl.tag)

  let new_lit = S.new_lit
  let new_atom = S.new_atom

  let export (st:t) : St.clause export =
    let hyps = S.hyps st in
    let history = S.history st in
    let local = S.temp st in
    {hyps; history; local}

  module Clause = struct
    include St.Clause

    let atoms c = St.Clause.atoms c |> Array.map (fun a -> a.St.lit)

    let make st ?tag l =
      let l = List.map (S.mk_atom st) l in
      St.Clause.make ?tag l St.Hyp
  end

  module Formula = St.Formula
  module Term = St.Term
end
