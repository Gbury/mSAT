(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module Fsmt : sig
    include Formula_intf.S
    val mk_prop : int -> t
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

