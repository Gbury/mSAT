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

module type S = Solver_types_intf.S

module Make : functor (E : Expr_intf.S)(Th : Theory_intf.S)
  -> S with type formula = E.Formula.t and type proof = Th.proof
(** Functor to instantiate the types of clauses for the Solver. *)
