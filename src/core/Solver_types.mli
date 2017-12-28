(**************************************************************************)
(*                                                                        *)
(*                                  Cubicle                               *)
(*             Combining model checking algorithms and SMT solvers        *)
(*                                                                        *)
(*                  Sylvain Conchon and Alain Mebsout                     *)
(*                  Universite Paris-Sud 11                               *)
(*                                                                        *)
(*  Copyright 2011. This file is distributed under the terms of the       *)
(*  Apache Software License version 2.0                                   *)
(*                                                                        *)
(**************************************************************************)
(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2016 Guillaume Bury
Copyright 2016 Simon Cruanes
*)

(** Internal types (implementation)

    This modules actually implements the internal types used by the solver.
    Since mutation is heavily used in the solver, it is really, really, *really*
    discouraged to direclty use the functions in this module if you don't know the
    inner working of mSAT perfectly as even the simplest
    change can have dramatic effects on the solver.
*)

module type S = Solver_types_intf.S
(** Interface for the internal types. *)

module Var_fields = Solver_types_intf.Var_fields

module McMake (E : Expr_intf.S)(Dummy : sig end):
    S with type term = E.Term.t and type formula = E.Formula.t and type proof = E.proof
(** Functor to instantiate the types of clauses for a solver. *)

module SatMake (E : Formula_intf.S)(Dummy : sig end):
    S with type term = E.t and type formula = E.t and type proof = E.proof
(** Functor to instantiate the types of clauses for a solver. *)

