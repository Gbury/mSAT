(**************************************************************************)
(*                                                                        *)
(*                                  Cubicle                               *)
(*             Combining model checking algorithms and SMT solvers        *)
(*                                                                        *)
(*                  Sylvain Conchon, Evelyne Contejean                    *)
(*                  Francois Bobot, Mohamed Iguernelala, Alain Mebsout    *)
(*                  CNRS, Universite Paris-Sud 11                         *)
(*                                                                        *)
(*  Copyright 2011. This file is distributed under the terms of the       *)
(*  Apache Software License version 2.0                                   *)
(*                                                                        *)
(**************************************************************************)

module type S = sig
  type t

  module R : Sig.X

  val empty :  t
  val add : t -> Term.t -> t * Literal.LT.t list

  val mem : t -> Term.t -> bool

  val find : t -> Term.t -> R.r * Explanation.t

  val find_r : t -> R.r -> R.r * Explanation.t

  val union : 
    t -> R.r -> R.r -> Explanation.t -> 
    t * (R.r * (R.r * R.r * Explanation.t) list * R.r) list

  val distinct : t -> R.r list -> Explanation.t -> t

  val are_equal : t -> Term.t -> Term.t -> Sig.answer
  val are_distinct : t -> Term.t -> Term.t -> Sig.answer
  val already_distinct : t -> R.r list -> bool

  val class_of : t -> Term.t -> Term.t list
end

module Make ( X : Sig.X ) : S with module R = X
