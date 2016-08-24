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

open Msat

type t

type view = private {
  head: head;
  ty: Ty.t;
  mutable tag: int;
}

and head =
  | App of Symbols.t * t list
  | Ite of t * t * t

val view : t -> view
val head : t -> head

val app : Symbols.t -> t list -> Ty.t -> t
val const : Symbols.t -> Ty.t -> t
val ite : t -> t -> t -> t

val true_ : t
val false_ : t

val compare : t -> t -> int
val equal : t -> t -> bool
val hash : t -> int

val print : Format.formatter -> t -> unit

module Map : Map.S with type key = t
module Set : Set.S with type elt = t

