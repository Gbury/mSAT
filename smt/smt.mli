(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module Fsmt : sig
    include Formula_intf.S
    val mk_eq : string -> string -> t
    val mk_neq : string -> string -> t
end

module Tseitin : Tseitin.S with type atom = Fsmt.t

module Make(Dummy: sig end) : sig
  (** Fonctor to make a pure SAT Solver module with built-in literals. *)

  type atom = Fsmt.t
  (** Type for atoms, i.e boolean literals. *)

  type clause
  (** Abstract type for clauses *)

  type proof
  (** Abstract type for resolution proofs *)

  type res = Sat | Unsat
  (** Type of results returned by the solve function. *)

  val make_eq : string -> string -> atom
  (** Returns the literal corresponding to equality of the given variables
      @raise Invalid_var if given [0] as argument.*)

  val make_neq : string -> string -> atom
  (** Returns the literal corresponding to disequality of the given variables
      @raise Invalid_var if given [0] as argument.*)

  val neg : atom -> atom
  (** [neg a] returns the negation of a literal. Involutive, i.e [neg (neg a) = a]. *)

  val hash : atom -> int
  val equal : atom -> atom -> bool
  val compare : atom -> atom -> int
  (** Usual hash and comparison functions. For now, directly uses
      [Pervasives] and [Hashtbl] builtins. *)

  val solve : unit -> res
  (** Returns the satisfiability status of the current set of assumptions. *)

  val assume : atom list list -> unit
  (** Add a list of clauses to the set of assumptions. *)

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

