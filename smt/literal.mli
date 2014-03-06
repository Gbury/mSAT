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

module type OrderedType = sig
  type t
  val compare : t -> t -> int
  val hash :  t -> int
  val print : Format.formatter -> t -> unit
end

type 'a view = 
  | Eq of 'a * 'a 
  | Distinct of bool * 'a list
  | Builtin of bool * Hstring.t * 'a list

module type S = sig
  type elt
  type t

  val make : elt view -> t
  val view : t -> elt view

  val neg : t -> t

  val add_label : Hstring.t -> t -> unit
  val label : t -> Hstring.t

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int

  module Map : Map.S with type key = t
  module Set : Set.S with type elt = t

end

module Make ( X : OrderedType ) : S with type elt = X.t

module type S_Term = sig

  include S with type elt = Term.t

  val mk_pred : Term.t -> t
  val vrai : t
  val faux : t

end

module LT : S_Term


