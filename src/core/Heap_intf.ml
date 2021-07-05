
module type ELEMENTS = sig
  type t
  (** The heap itself *)

  type elt
  (** Element of the heap *)

  val size : t -> int
  (** Number of elements in the heap *)

  val get : t -> int -> elt
  (** [get h i] gets the [i]-th element.
      Precond: [i < size h] *)

  val elt_idx : t -> elt -> int
  (** Index in heap of the element. return [-1] if not set *)

  val set_elt_idx : t -> elt -> int -> unit
  (** Update index in heap *)

  val cmp_elt : t -> elt -> elt -> bool
  (** [cmp_elt h e1 e2] is true iff [e1] comes before [e2] *)
end

module type S = sig
  type elt
  (** Type of elements *)

  type t
  (** Eeap of {!elt}, whose priority is increased or decreased
      incrementally (see {!decrease} for instance) *)

  val in_heap : t -> elt -> bool

  val decrease : t -> elt -> unit
  (** [decrease h x] decreases the value associated to [x] within [h] *)

  (*val increase : (int -> int -> bool) -> t -> int -> unit*)

  val is_empty : t -> bool

  val clear : t -> unit
  (** Clear the content of the heap *)

  val insert : t -> elt -> unit
  (** Insert a new element into the heap *)

  val remove : t -> elt -> unit
  (** remove element *)

  (*val update : (int -> int -> bool) -> t -> int -> unit*)

  val remove_min : t -> elt
  (** Remove and return the integer that has the lowest value from the heap
      @raise Not_found if the heap is empty *)

  (*
  val filter : t -> (elt -> bool) -> unit
  (** Filter out values that don't satisfy the predicate *)
     *)
end
