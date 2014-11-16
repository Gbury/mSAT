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

  type formula
  type proof

  type var = {
    vid : int;
    pa : atom;
    na : atom;
    mutable weight : float;
    mutable seen : bool;
    mutable level : int;
    mutable reason : reason;
    mutable vpremise : premise
  }

  and atom = {
    var : var;
    lit : formula;
    neg : atom;
    mutable watched : clause Vec.t;
    mutable is_true : bool;
    aid : int
  }

  and clause = {
    name : string;
    atoms : atom Vec.t;
    mutable activity : float;
    mutable removed : bool;
    learnt : bool;
    cpremise : premise
  }

  and reason = clause option
  and premise =
      | History of clause list
      | Lemma of proof
  (** Recursive types for literals (atoms) and clauses *)

  val dummy_var : var
  val dummy_atom : atom
  val dummy_clause : clause
  (** Dummy values for use in vector dummys *)

  val empty_clause : clause
  (** The empty clause *)

  val add_atom : formula -> atom
  (** Returns the atom associated with the given formula *)

  val make_var : formula -> var * bool
  (** Returns the variable linked with the given formula, and wether the atom associated with the formula
      is [var.pa] or [var.na] *)

  val make_clause : string -> atom list -> int -> bool -> premise -> clause
  (** [make_clause name atoms size learnt premise] creates a clause with the given attributes. *)

  val nb_vars : unit -> int
  val get_var : int -> var
  (** Read access to the vector of variables created *)

  val fresh_name : unit -> string
  val fresh_lname : unit -> string
  val fresh_dname : unit -> string
  (** Fresh names for clauses *)

  val clear : unit -> unit
  (** Forget all variables cretaed *)

  val print_atom : Format.formatter -> atom -> unit
  val print_clause : Format.formatter -> clause -> unit
  (** Pretty printing functions for atoms and clauses *)

  val pp_atom : Buffer.t -> atom -> unit
  val pp_clause : Buffer.t -> clause -> unit
  (** Debug function for atoms and clauses (very verbose) *)

end

