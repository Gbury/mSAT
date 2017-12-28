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

val from_list : 'a list -> 'a -> 'a t

val to_list : 'a t -> 'a list
(** Returns the list of elements of the vector *)

val clear : 'a t -> unit
(** Set size to 0, doesn't free elements *)

val shrink : 'a t -> int -> unit
(** [shrink vec sz] resets size of [vec] to [sz].
    Assumes [sz >=0 && sz <= size vec] *)

val pop : 'a t -> unit
(** Pop last element
    @raise Invalid_argument if the vector is empty *)

val size : 'a t -> int

val is_empty : 'a t -> bool

val grow_to_exact : 'a t -> int -> unit

val grow_to_double_size : 'a t -> unit

val grow_to_at_least : 'a t -> int -> unit
(** [grow_to_at_least vec n] ensures that [capacity vec >= n] in
    the most efficient way *)

val is_full : 'a t -> bool
(** Is the capacity of the vector equal to the number of its elements? *)

val push : 'a t -> 'a -> unit

val append : 'a t -> 'a t -> unit
(** [append v1 v2] pushes all elements of [v2] into [v1] *)

val last : 'a t -> 'a
(** Last element, or
    @raise Invalid_argument if the vector is empty *)

val pop_last : 'a t -> 'a
(** Combine {!last} and {!pop}: remove last element and return it
    @raise Invalid_argument if empty *)

val get : 'a t -> int -> 'a
(** get the element at the given index, or
    @raise Invalid_argument if the index is not valid *)

val set : 'a t -> int -> 'a -> unit
(** set the element at the given index, either already set or the first
    free slot if [not (is_full vec)], or
    @raise Invalid_argument if the index is not valid *)

val copy : 'a t -> 'a t
(** Fresh copy *)

val move_to : 'a t -> 'a t -> unit
(** [move_to a b] copies the content of [a] to [b], discarding [b]'s old content *)

val remove : 'a t -> 'a -> unit
(** Uses [(==)] for comparison *)

val fast_remove : 'a t -> int -> unit
(** Remove element at index [i] without preserving order
    (swap with last element) *)

val filter_in_place : ('a -> bool) -> 'a t -> unit
(** [filter_in_place f v] removes from [v] the elements that do
    not satisfy [f] *)

val sort : 'a t -> ('a -> 'a -> int) -> unit
(** Sort in place the array *)

val iter : ('a -> unit) -> 'a t -> unit
(** Iterate on elements *)

val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
(** Fold over elements *)

val exists : ('a -> bool) -> 'a t -> bool
(** Does there exist an element that satisfies the predicate? *)

val for_all : ('a -> bool) -> 'a t -> bool
(** Do all elements satisfy the predicate? *)

val print :
  ?sep:string ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a t -> unit
