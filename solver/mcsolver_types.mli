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

module type S = Mcsolver_types_intf.S

module Make : functor (L : Log_intf.S)(E : Expr_intf.S)(Th : Plugin_intf.S with
    type term = E.Term.t and type formula = E.Formula.t)
  -> S with type term = E.Term.t and type formula = E.Formula.t and type proof = Th.proof
(** Functor to instantiate the types of clauses for the Solver. *)
