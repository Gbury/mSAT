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

val dummy : t

val fresh : unit -> t

val mk_prop : int -> t
val mk_eq : var -> var -> t
val mk_neq : var -> var -> t

val neg : t -> t
val norm : t -> t * bool

val hash : t -> int
val equal : t -> t -> bool
val compare : t -> t -> int

val label : t -> Hstring.t
val add_label : Hstring.t -> t -> unit

val print : Format.formatter -> t -> unit

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
