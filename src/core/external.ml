(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2016 Guillaume Bury
Copyright 2016 Simon Cruanes
*)

module type S = Solver_intf.S

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

  type res = Sat | Unsat

  let assume ?tag l =
    try S.assume ?tag l
    with S.Unsat -> ()

  let solve () =
    try
      S.solve ();
      Sat
    with S.Unsat -> Unsat

  let eval = S.eval
  let eval_level = S.eval_level

  let get_proof () =
    match S.unsat_conflict () with
    | None -> assert false
    | Some c -> S.Proof.prove_unsat c

  let unsat_core = S.Proof.unsat_core

  let get_tag cl = St.(cl.tag)

  (* Push/pop operations *)
  type level = S.level
  let base_level = S.base_level
  let current_level = S.current_level
  let push = S.push
  let pop = S.pop
  let reset = S.reset

end
