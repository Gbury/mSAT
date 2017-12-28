
(** The module defining formulas *)

(** SAT Formulas

    This modules implements formuals adequate for use in a pure SAT Solver.
    Atomic formuals are represented using integers, that should allow
    near optimal efficiency (both in terms of space and time).
*)

include Formula_intf.S
(** This modules implements the requirements for implementing an SAT Solver. *)

val make : int -> t
(** Make a proposition from an integer. *)

val fresh : unit -> t
(** Make a fresh atom *)

val compare : t -> t -> int
(** Compare atoms *)

val sign : t -> bool
(** Is the given atom positive ? *)

val apply_sign : bool -> t -> t
(** [apply_sign b] is the identity if [b] is [true], and the negation
    function if [b] is [false]. *)

val set_sign : bool -> t -> t
(** Return the atom with the sign set. *)
