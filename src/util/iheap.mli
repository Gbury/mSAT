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

type t
(** Heap of integers, whose priority is increased or decreased
    incrementally (see {!decrease} for instance) *)

val init : int -> t
(** Create a heap with the given number of values inside.
    [init len] contains integers from [0] to [len-1]. *)

val in_heap : t -> int -> bool
(** [in_heap h x] returns [true] iff [x] is among the integers that belong to
    the heap. *)

val decrease : (int -> int -> bool) -> t -> int -> unit
(** [decrease cmp h x] decreases the value associated to [x] within [h],
    according to the comparison function [cmp] *)

(*val increase : (int -> int -> bool) -> t -> int -> unit*)

val size : t -> int
(** Number of integers within the heap *)

val is_empty : t -> bool

val clear : t -> unit
(** Clear the content of the heap *)

val insert : (int -> int -> bool) -> t -> int -> unit
(** Insert a new integer into the heap, according to the given comparison *)

val grow_to_at_least: t -> int -> unit
(** Hint: augment the internal capacity of the heap until it reaches at
    least the given integer *)

(*val update : (int -> int -> bool) -> t -> int -> unit*)

val remove_min : (int -> int -> bool) -> t -> int
(** Remove and return the integer that has the lowest value from the heap
    @raise Not_found if the heap is empty *)

val filter : t -> (int -> bool) -> (int -> int -> bool) -> unit
(** Filter out values that don't satisfy the predicate. A comparison
    function is used to re-order the heap *)
