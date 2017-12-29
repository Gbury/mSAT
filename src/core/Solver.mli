(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

(** mSAT safe interface

    This module defines a safe interface for the core solver.
    It is the basis of the {!module:Solver} and {!module:Mcsolver} modules.
*)

module type S = Solver_intf.S
(** Safe external interface of solvers. *)

module Make
    (St : Solver_types.S)
    (Th : Plugin_intf.S with type term = St.term
                         and type formula = St.formula
                         and type proof = St.proof)
  : S with type term = St.term
       and type formula = St.formula
       and type clause = St.clause
       and type Proof.lemma = St.proof
(** Functor to make a safe external interface. *)


