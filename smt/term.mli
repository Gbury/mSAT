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

type t
type view = private {f: Symbols.t ; xs: t list; ty: Ty.t; tag : int}

val view : t -> view
val make : Symbols.t -> t list -> Ty.t -> t

val vrai : t
val faux : t
val int : string -> t
val real : string -> t

val is_int : t -> bool
val is_real : t -> bool

val compare : t -> t -> int
val equal : t -> t -> bool
val hash : t -> int

val print : Format.formatter -> t -> unit

module Map : Map.S with type key = t
module Set : Set.S with type elt = t

