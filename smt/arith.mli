(**************************************************************************)
(*                                                                        *)
(*                                  Cubicle                               *)
(*             Combining model checking algorithms and SMT solvers        *)
(*                                                                        *)
(*                  Sylvain Conchon, Alain Mebsout                        *)
(*                  Francois Bobot, Mohamed Iguernelala                   *)
(*                  Universite Paris-Sud 11                               *)
(*                                                                        *)
(*  Copyright 2011. This file is distributed under the terms of the       *)
(*  Apache Software License version 2.0                                   *)
(*                                                                        *)
(**************************************************************************)

module Type (X : Sig.X ): Polynome.T with type r = X.r

module Make 
  (X : Sig.X)
  (P : Polynome.T with type r = X.r)
  (C : Sig.C with type t = P.t and type r = X.r) : Sig.THEORY 
  with type r = X.r and type t = P.t
