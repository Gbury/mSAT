(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module Fsat : Formula_intf.S with type t = int

module Tseitin : Tseitin.S with type atom = Fsat.t

module Make(Dummy: sig end) : sig
  (** Fonctor to make a pure SAT Solver module with built-in literals. *)

  exception Bad_atom
  (** Exception raised when a problem with atomic formula encoding is detected. *)

  type atom = Fsat.t
  (** Type for atoms, i.e boolean literals. *)

  type clause
  (** Abstract type for clauses *)

  type proof
  (** Abstract type for resolution proofs *)

  type res = Sat | Unsat
  (** Type of results returned by the solve function. *)

  val new_atom : unit -> atom
  (** [new_atom ()] returns a fresh literal.
      @raise Bad_atom if there are no more integer available to represent new atoms. *)

  val make : int -> atom
  (** Returns the literal corresponding to the integer.
      @raise Bad_atom if given [0] as argument.*)

  val neg : atom -> atom
  (** [neg a] returns the negation of a literal. Involutive, i.e [neg (neg a) = a]. *)

  val hash : atom -> int
  val equal : atom -> atom -> bool
  val compare : atom -> atom -> int
  (** Usual hash and comparison functions. For now, directly uses
      [Pervasives] and [Hashtbl] builtins. *)

  val iter_atoms : (atom -> unit) -> unit
  (** Allows iteration over all atoms created (even if unused). *)

  val solve : unit -> res
  (** Returns the satisfiability status of the current set of assumptions. *)

  val eval : atom -> bool
  (** Return the current assignement of the literals. *)

  val assume : ?tag:int -> atom list list -> unit
  (** Add a list of clauses to the set of assumptions. *)

  val tag_clause : clause -> int option
  (** Returns the tag associated with the clause. *)

  val get_proof : unit -> proof
  (** Returns the resolution proof found, if [solve] returned [Unsat]. *)

  val unsat_core : proof -> clause list
  (** Returns the unsat-core of the proof. *)

  val print_atom : Format.formatter -> atom -> unit
  (** Print the atom on the given formatter. *)

  val print_clause : Format.formatter -> clause -> unit
  (** Print the clause on the given formatter. *)

  val print_proof : Format.formatter -> proof -> unit
  (** Print the given proof in dot format. *)

end

