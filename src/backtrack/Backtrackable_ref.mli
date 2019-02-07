
(** {1 Backtrackable ref} *)

type 'a t

val create : ?copy:('a -> 'a) -> 'a -> 'a t
(** Create a backtrackable reference holding the given value initially.
    @param copy if provided, will be used to copy the value when [push_level]
    is called. *)

val set : 'a t -> 'a -> unit
(** Set the reference's current content *)

val get : 'a t -> 'a
(** Get the reference's current content *)

val update : 'a t -> ('a -> 'a) -> unit
(** Update the reference's current content *)
  
val push_level : _ t -> unit
(** Push a backtracking level, copying the current value on top of some
    stack. The [copy] function will be used if it was provided in {!create}. *)

val n_levels : _ t -> int
(** Number of saved values *)

val pop_levels : _ t -> int -> unit
(** Pop [n] levels, restoring to the value the reference was storing [n] calls
    to [push_level] earlier.
    @raise Invalid_argument if [n] is bigger than [n_levels]. *)
