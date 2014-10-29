(**************************************************************************)
(*                                                                        *)
(*                                  Cubicle                               *)
(*             Combining model checking algorithms and SMT solvers        *)
(*                                                                        *)
(*                  Guillaume Bury                                        *)
(*                  INRIA                                                 *)
(*                  Sylvain Conchon and Alain Mebsout                     *)
(*                  Universite Paris-Sud 11                               *)
(*                                                                        *)
(*  Copyright 2011. This file is distributed under the terms of the       *)
(*  Apache Software License version 2.0                                   *)
(*                                                                        *)
(**************************************************************************)

module type S = sig

  type t

  val vrai : t
  val faux : t
  val dummy : t

  val neg : t -> t
  val norm : t -> t * bool

  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int

  val label : t -> Hstring.t
  val add_label : Hstring.t -> t -> unit

  val print : Format.formatter -> t -> unit

  module Map : Map.S with type key = t
  module Set : Set.S with type elt = t

end

