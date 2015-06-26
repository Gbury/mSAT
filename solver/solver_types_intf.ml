(**************************************************************************)
(*                                                                        *)
(*                                  Cubicle                               *)
(*             Combining model checking algorithms and SMT solvers        *)
(*                                                                        *)
(*                  Sylvain Conchon and Alain Mebsout                     *)
(*                  Universite Paris-Sud 11                               *)
(*                                                                        *)
(*  Copyright 2011. This file is distributed under the terms of the       *)
(*  Apache Software License version 2.0                                   *)
(*                                                                        *)
(**************************************************************************)

module type S = sig
  (** The signatures of clauses used in the Solver. *)

  val mcsat : bool

  type term
  type formula
  type proof

  type lit = {
    lid : int;
    term : term;
    mutable level : int;
    mutable weight : float;
    mutable assigned : term option;
  }

  type var = {
    vid : int;
    pa : atom;
    na : atom;
    mutable seen : bool;
    mutable level : int;
    mutable weight : float;
    mutable reason : reason;
  }

  and atom = {
    aid : int;
    var : var;
    neg : atom;
    lit : formula;
    mutable is_true : bool;
    mutable watched : clause Vec.t;
  }

  and clause = {
    name : string;
    tag : int option;
    atoms : atom Vec.t;
    learnt : bool;
    cpremise : premise;
    mutable activity : float;
    mutable removed : bool;
  }

  and reason =
    | Semantic of int
    | Bcp of clause option

  and premise =
    | History of clause list
    | Lemma of proof

  type elt = (lit, var) Either.t
  (** Recursive types for literals (atoms) and clauses *)

  val dummy_var : var
  val dummy_atom : atom
  val dummy_clause : clause
  (** Dummy values for use in vector dummys *)

  val nb_vars : unit -> int
  val get_var : int -> elt
  val iter_vars : (elt -> unit) -> unit
  (** Read access to the vector of variables created *)

  val add_atom : formula -> atom
  (** Returns the atom associated with the given formula *)
  val add_term : term -> lit
  (** Returns the variable associated with the term *)
  val make_boolean_var : formula -> var * bool
  (** Returns the variable linked with the given formula, and wether the atom associated with the formula
      is [var.pa] or [var.na] *)

  val iter_sub : (lit -> unit) -> var -> unit
  (** Iterates over the semantic var corresponding to subterms of the fiven literal, according
      to Th.iter_assignable *)

  val empty_clause : clause
  (** The empty clause *)
  val make_clause : ?tag:int -> string -> atom list -> int -> bool -> premise -> clause
  (** [make_clause name atoms size learnt premise] creates a clause with the given attributes. *)

  val fresh_name : unit -> string
  val fresh_lname : unit -> string
  val fresh_tname : unit -> string
  val fresh_hname : unit -> string
  (** Fresh names for clauses *)

  val proof_debug : proof -> string * (atom list) * (lit list) * (string option)
  (** Debugging info for proofs (see Plugin_intf). *)

  val print_lit : Format.formatter -> lit -> unit
  val print_atom : Format.formatter -> atom -> unit
  val print_clause : Format.formatter -> clause -> unit
  (** Pretty printing functions for atoms and clauses *)

  val pp_lit : Buffer.t -> lit -> unit
  val pp_atom : Buffer.t -> atom -> unit
  val pp_clause : Buffer.t -> clause -> unit
  (** Debug function for atoms and clauses (very verbose) *)

end

