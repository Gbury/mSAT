
type 'a t
(** Abstract type of vectors of 'a *)

val make : int -> 'a -> 'a t
(** [make cap dummy] creates a new vector filled with [dummy]. The vector
    is initially empty but its underlying array has capacity [cap].
    [dummy] will stay alive as long as the vector *)

val create : unit -> 'a t

val to_list : 'a t -> 'a list
(** Returns the list of elements of the vector *)

val to_array : 'a t -> 'a array

val of_list : 'a list -> 'a t

val to_seq : 'a t -> 'a Iter.t

val clear : 'a t -> unit
(** Set size to 0, doesn't free elements *)

val shrink : 'a t -> int -> unit
(** [shrink vec sz] resets size of [vec] to [sz].
    Assumes [sz >=0 && sz <= size vec] *)

val pop : 'a t -> 'a
(** Pop last element and return it.
    @raise Invalid_argument if the vector is empty *)

val size : 'a t -> int

val is_empty : 'a t -> bool

val is_full : 'a t -> bool
(** Is the capacity of the vector equal to the number of its elements? *)

val push : 'a t -> 'a -> unit
(** Push element into the vector *)

val get : 'a t -> int -> 'a
(** get the element at the given index, or
    @raise Invalid_argument if the index is not valid *)

val set : 'a t -> int -> 'a -> unit
(** set the element at the given index, either already set or the first
    free slot if [not (is_full vec)], or
    @raise Invalid_argument if the index is not valid *)

val copy : 'a t -> 'a t
(** Fresh copy *)

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

val iteri : (int -> 'a -> unit) -> 'a t -> unit
(** Iterate on elements with their index *)

val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
(** Fold over elements *)

val exists : ('a -> bool) -> 'a t -> bool
(** Does there exist an element that satisfies the predicate? *)

val for_all : ('a -> bool) -> 'a t -> bool
(** Do all elements satisfy the predicate? *)

val pp :
  ?sep:string ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a t -> unit
