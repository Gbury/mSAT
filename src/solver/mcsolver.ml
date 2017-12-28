(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module type S = Solver_intf.S

module Make (E : Expr_intf.S)
    (Th : Plugin_intf.S with type term = E.Term.t
                         and type formula = E.Formula.t
                         and type proof = E.proof)
    () =
  External.Make
    (Solver_types.McMake(E)(struct end))
    (Th)
    ()


