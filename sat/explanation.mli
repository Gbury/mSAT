(**************************************************************************)
(*                                                                        *)
(*                                  Cubicle                               *)
(*             Combining model checking algorithms and SMT solvers        *)
(*                                                                        *)
(*                  Sylvain Conchon and Alain Mebsout                     *)
(*                  Stephane Lescuyer                                     *)
(*                  INRIA, Universite Paris-Sud 11                        *)
(*                                                                        *)
(*  Copyright 2011. This file is distributed under the terms of the       *)
(*  Apache Software License version 2.0                                   *)
(*                                                                        *)
(**************************************************************************)

module type S = Explanation_intf.S

module Make : functor (St : Solver_types.S) -> S with type atom = St.atom
(** Functor to create the types of explanations used in the Solver Module. *)
