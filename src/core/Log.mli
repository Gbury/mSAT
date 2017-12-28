(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

(** {1 Logging function, for debugging} *)

val enabled : bool

val set_debug : int -> unit (** Set debug level *)
val get_debug : unit -> int (** Current debug level *)

val debugf :
  int ->
  ((('a, Format.formatter, unit, unit) format4 -> 'a) -> unit) ->
  unit
(** Emit a debug message at the given level. If the level is lower
    than [get_debug ()], the message will indeed be emitted *)

val debug : int -> string -> unit
(** Simpler version of {!debug}, without formatting *)

val set_debug_out : Format.formatter -> unit
(** Change the output formatter. *)

