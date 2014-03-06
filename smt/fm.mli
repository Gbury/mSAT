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

module type EXTENDED_Polynome = sig
  include Polynome.T
  val poly_of : r -> t
  val alien_of : t -> r
end

module Make 
  (X : Sig.X)
  (P : EXTENDED_Polynome with type r = X.r) 
  : Sig.RELATION with type r = X.r
