(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

(** Create McSat solver

    This module provides a functor to create an McSAt solver.
*)

module type S = Msat.S
(** The interface exposed by the solver. *)

module Make (E : Expr_intf.S)
    (Th : Plugin_intf.S with type term = E.Term.t
                         and type formula = E.Formula.t
                         and type proof = E.proof)
    () :
  S with type St.term = E.Term.t
     and type St.formula = E.Formula.t
     and type St.proof = E.proof
(** Functor to create a solver parametrised by the atomic formulas and a theory. *)

