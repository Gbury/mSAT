
(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2016 Guillaume Bury
Copyright 2016 Simon Cruanes
*)

(** {1 Unique Identifiers} *)

type t

val make : string -> t
val copy : t -> t

val to_string : t -> string

val equal : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int

val pp_name : Format.formatter -> t -> unit

module Map : Map.S with type key = t
module Set : Set.S with type elt = t
module Tbl : Hashtbl.S with type key = t
