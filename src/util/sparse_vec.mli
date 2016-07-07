
(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

(** {1 Sparse vector, filled with default value} *)

type 'a t
(** Abstract type of sparse vectors of 'a *)

val make : int -> 'a -> 'a t
(** [make cap default] creates a new vector filled with [default]. The vector's
    initial length is [cap] *)

val init : int -> (int -> 'a) -> 'a -> 'a t
(** Same as {!Array.init}, but also with a default element *)

val length : 'a t -> int
(** Range of valid indices *)

val resize : 'a t -> int -> unit
(** Set the length of the array to [i] *)

val incr : 'a t -> unit
(** Increment length size by one *)

val decr : 'a t -> unit
(** Decrement length by one *)

val is_empty : 'a t -> bool
(** Check whether length=0 *)

val clear : 'a t -> unit
(** Set length to 0 *)

val get : 'a t -> int -> 'a
(** get the element at the given index, or
    @raise Invalid_argument if the index is not valid *)

val set : 'a t -> int -> 'a -> unit
(** set the element at the given index, either already set or the first
    free slot if [not (is_full vec)], or
    @raise Invalid_argument if the index is not valid *)
