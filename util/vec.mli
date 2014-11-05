(**************************************************************************)
(*                                                                        *)
(*                                  Cubicle                               *)
(*             Combining model checking algorithms and SMT solvers        *)
(*                                                                        *)
(*                  Mohamed Iguernelala                                   *)
(*                  Universite Paris-Sud 11                               *)
(*                                                                        *)
(*  Copyright 2011. This file is distributed under the terms of the       *)
(*  Apache Software License version 2.0                                   *)
(*                                                                        *)
(**************************************************************************)

type 'a t
(** Abstract type of vectors of 'a *)

val make : int -> 'a -> 'a t
(** [make cap dummy] creates a new vector filled with [dummy]. The vector
    is initially empty but its underlying array has capacity [cap].
    [dummy] will stay alive as long as the vector *)

val make_empty : 'a -> 'a t
(** Vector with an empty capacity. The only argument is the dummy. *)

val init : int -> (int -> 'a) -> 'a -> 'a t
(** Same as {!Array.init}, but also with a dummy element *)

val from_array : 'a array -> int -> 'a -> 'a t
(** [from_array arr size dummy] takes ownership of [data] (no copy)
    to create a vector. [size] is the length of the slice of [data] that is
    used ([size <= Array.length data] must hold) *)

val from_list : 'a list -> int -> 'a -> 'a t

val clear : 'a t -> unit
(** Set size to 0, doesn't free elements *)

val shrink : 'a t -> int -> unit

val pop : 'a t -> unit
(** Pop last element
    @raise Invalid_argument if the vector is empty *)

val size : 'a t -> int

val is_empty : 'a t -> bool

val grow_to : 'a t -> int -> unit

val grow_to_double_size : 'a t -> unit

val grow_to_by_double : 'a t -> int -> unit

val is_full : 'a t -> bool
(** Is the capacity of the vector equal to the number of its elements? *)

val push : 'a t -> 'a -> unit

val last : 'a t -> 'a
(** Last element, or
    @raise Invalid_argument if the vector is empty *)

val get : 'a t -> int -> 'a
(** get the element at the given index, or
    @raise Invalid_argument if the index is not valid *)

val set : 'a t -> int -> 'a -> unit
(** set the element at the given index, either already set or the first
    free slot if [not (is_full vec)], or
    @raise Invalid_argument if the index is not valid *)

val set_unsafe : 'a t -> int -> 'a -> unit
(* undocumented. TODO remove asap *)

val copy : 'a t -> 'a t
(** Fresh copy *)

val move_to : 'a t -> 'a t -> unit
(** [move_to a b] copies the content of [a] to [b], discarding [b]'s old content *)

val remove : 'a t -> 'a -> unit
(** Uses [(==)] for comparison *)

val fast_remove : 'a t -> 'a -> unit
(** Remove element without preserving order (swap with last element) *)

val sort : 'a t -> ('a -> 'a -> int) -> unit
(** Sort in place the array *)

val iter : ('a -> unit) -> 'a t -> unit
(** Iterate on elements *)

val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
(** Fold over elements *)

val exists : ('a -> bool) -> 'a t -> bool
(** Does there exist an element that satisfies the predicate? *)

