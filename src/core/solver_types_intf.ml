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

  (** {2 Type definitions} *)

  type term
  type formula
  type proof

  type lit = {
    lid : int;
    term : term;
    mutable l_level : int;
    mutable l_weight : float;
    mutable assigned : term option;
  }

  type var = {
    vid : int;
    pa : atom;
    na : atom;
    mutable seen : bool;
    mutable v_level : int;
    mutable v_weight : float;
    mutable v_assignable: lit list option;
    mutable reason : reason option;
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
    atoms : atom array;
    mutable cpremise : premise;
    mutable activity : float;
    mutable attached : bool;
    mutable visited : bool;
  }

  and reason =
    | Decision
    | Bcp of clause
    | Semantic of int

  and premise =
    | Hyp
    | Lemma of proof
    | History of clause list

  (** {2 Decisions and propagations} *)
  type t =
    | Lit of lit
    | Atom of atom
  (** Either a lit of an atom *)

  val of_lit : lit -> t
  val of_atom : atom -> t
  (** Constructors and destructors *)

  (** {2 Elements} *)

  type elt =
    | E_lit of lit
    | E_var of var
  (** Either a lit of a var *)

  val nb_elt : unit -> int
  val get_elt : int -> elt
  val iter_elt : (elt -> unit) -> unit
  (** Read access to the vector of variables created *)

  val elt_of_lit : lit -> elt
  val elt_of_var : var -> elt
  (** Constructors & destructor for elements *)

  val get_elt_id : elt -> int
  val get_elt_level : elt -> int
  val get_elt_weight : elt -> float
  val set_elt_level : elt -> int -> unit
  val set_elt_weight : elt -> float -> unit
  (** Accessors for elements *)

  (** {2 Variables, Litterals & Clauses } *)

  val dummy_var : var
  val dummy_atom : atom
  val dummy_clause : clause
  (** Dummy values for use in vector dummys *)

  val add_term : term -> lit
  (** Returns the variable associated with the term *)
  val add_atom : formula -> atom
  (** Returns the atom associated with the given formula *)
  val make_boolean_var : formula -> var * Formula_intf.negated
  (** Returns the variable linked with the given formula, and wether the atom associated with the formula
      is [var.pa] or [var.na] *)

  val empty_clause : clause
  (** The empty clause *)
  val make_clause : ?tag:int -> string -> atom list -> premise -> clause
  (** [make_clause name atoms size premise] creates a clause with the given attributes. *)

  (** {2 Clause names} *)

  val fresh_name : unit -> string
  val fresh_lname : unit -> string
  val fresh_tname : unit -> string
  val fresh_hname : unit -> string
  (** Fresh names for clauses *)

  (** {2 Printing} *)

  val print_lit : Format.formatter -> lit -> unit
  val print_atom : Format.formatter -> atom -> unit
  val print_clause : Format.formatter -> clause -> unit
  (** Pretty printing functions for atoms and clauses *)

  val pp_lit : Format.formatter -> lit -> unit
  val pp_atom : Format.formatter -> atom -> unit
  val pp_clause : Format.formatter -> clause -> unit
  (** Debug function for atoms and clauses (very verbose) *)

end

