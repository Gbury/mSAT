(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

open Msat

module Fsat : sig

  include Formula_intf.S with type t = private int

  exception Bad_atom
  (** Exception raised when a problem with atomic formula encoding is detected. *)

  val make : int -> t
  (** Returns the literal corresponding to the integer.
      @raise Bad_atom if given [0] as argument.*)

  val fresh : unit -> t
  (** [new_atom ()] returns a fresh literal.
      @raise Bad_atom if there are no more integer available to represent new atoms. *)

  val neg : t -> t
  (** [neg a] returns the negation of a literal. Involutive, i.e [neg (neg a) = a]. *)

  val abs : t -> t
  val sign : t -> bool
  val apply_sign : bool -> t -> t
  val set_sign : bool -> t -> t
  (** Convenienc functions for manipulating literals. *)

  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  (** Usual hash and comparison functions. For now, directly uses
      [Pervasives] and [Hashtbl] builtins. *)

  val iter : (t -> unit) -> unit
  (** Allows iteration over all atoms created (even if unused). *)

end

module Tseitin : Tseitin.S with type atom = Fsat.t

module Make(Dummy : sig end) : sig
  (** Fonctor to make a pure SAT Solver module with built-in literals. *)

  include Solver.S with type St.formula = Fsat.t

  val print_atom : Format.formatter -> atom -> unit
  (** Print the atom on the given formatter. *)

  val print_clause : Format.formatter -> St.clause -> unit
  (** Print the clause on the given formatter. *)

  val print_proof : Format.formatter -> Proof.proof -> unit
  (** Print the given proof in dot format. *)

  val print_dimacs : Format.formatter -> St.clause list -> unit
  (** Prints a cnf in dimacs format on the given formatter *)

end

