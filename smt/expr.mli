(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

type var = string
type formula = private
  | Prop of int
  | Equal of var * var
  | Distinct of var * var

type t = formula
type proof = unit

include Formula_intf.S with type t := formula and type proof := proof

val dummy : t

val fresh : unit -> t

val mk_prop : int -> t
val mk_eq : var -> var -> t
val mk_neq : var -> var -> t

module Term : sig
  type t = var
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val print : Format.formatter -> t -> unit
end
module Formula : sig
  type t = formula
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val print : Format.formatter -> t -> unit
end
