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

type answer = Yes of Explanation.t | No

type 'a literal =
  | LSem of 'a Literal.view
  | LTerm of Literal.LT.t

module type ELT = sig
  type r

  val make : Term.t -> r * Literal.LT.t list

  val type_info : r -> Ty.t

  val compare : r -> r -> int

  val equal : r -> r -> bool

  val hash : r -> int

  val leaves : r -> r list

  val subst : r -> r -> r -> r

  val solve : r -> r ->  (r * r) list

  val term_embed : Term.t -> r

  val term_extract : r -> Term.t option

  val unsolvable   : r -> bool

  val fully_interpreted : Symbols.t -> bool

  val print : Format.formatter -> r -> unit
end

module type S = sig
  type t

  module R : ELT

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

module Make ( X : ELT ) : S with module R = X
