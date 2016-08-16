(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module Make(T : OrderedType) : sig
  type t
  exception Unsat of T.t * T.t
  val empty : t
  val find : t -> T.t -> T.t
  val union : t -> T.t -> T.t -> t
  val forbid : t -> T.t -> T.t -> t
end

