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

type t

type r
(** representative *)

val empty :  t
val add : t -> Term.t -> t * Literal.LT.t list

val mem : t -> Term.t -> bool

val find : t -> Term.t -> r * Explanation.t

val find_r : t -> r -> r * Explanation.t

val union :
  t -> r -> r -> Explanation.t ->
  t * (r * (r * r * Explanation.t) list * r) list

val distinct : t -> r list -> Explanation.t -> t

val are_equal : t -> Term.t -> Term.t -> Sig.answer
val are_distinct : t -> Term.t -> Term.t -> Sig.answer
val already_distinct : t -> r list -> bool

val class_of : t -> Term.t -> Term.t list
