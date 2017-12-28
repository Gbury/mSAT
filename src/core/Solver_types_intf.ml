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
(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2016 Guillaume Bury
Copyright 2016 Simon Cruanes
*)

(** Internal types (interface)

    This modules defines the interface of most of the internal types
    used in the core solver.
*)

module Var_fields = BitField.Make()

module type S = sig
  (** The signatures of clauses used in the Solver. *)

  val mcsat : bool
  (** TODO:deprecate. *)

  (** {2 Type definitions} *)

  type term
  type formula
  type proof
  (** The types of terms, formulas and proofs. All of these are user-provided. *)

  type seen =
    | Nope
    | Both
    | Positive
    | Negative

  (* TODO: hide these types (from the outside of [Msat]);
     instead, provide well defined modules [module Lit : sig type t val …]
     that define their API in Msat itself (not here) *)

  type lit = {
    lid : int;                      (** Unique identifier *)
    term : term;                    (** Wrapped term *)
    mutable l_level : int;          (** Decision level of the assignment *)
    mutable l_idx: int;             (** index in heap *)
    mutable l_weight : float;       (** Weight (for the heap) *)
    mutable assigned : term option; (** Assignment *)
  }
  (** Wrapper type for literals, i.e. theory terms (for mcsat only). *)

  type var = {
    vid : int;  (** Unique identifier *)
    pa : atom;  (** Link for the positive atom *)
    na : atom;  (** Link for the negative atom *)
    mutable v_fields : Var_fields.t; (** bool fields *)
    mutable v_level : int;      (** Level of decision/propagation *)
    mutable v_idx: int;         (** rank in variable heap *)
    mutable v_weight : float;   (** Variable weight (for the heap) *)
    mutable v_assignable: lit list option;
    (** In mcsat, the list of lits that wraps subterms of the formula wrapped. *)
    mutable reason : reason option;
    (** The reason for propagation/decision of the literal *)
  }

  and atom = {
    aid : int;                      (** Unique identifier *)
    var : var;                      (** Link for the parent variable *)
    neg : atom;                     (** Link for the negation of the atom *)
    lit : formula;                  (** Wrapped formula *)
    mutable is_true : bool;         (** Is the atom true ? Conversely, the atom
                                        is false iff a.neg.is_true *)
    mutable watched : clause Vec.t; (** The vector of clauses that watch this atom *)
  }
  (** Atoms and variables wrap theory formulas. They exist in the form of
      triplet: a variable and two atoms. For a formula [f] in normal form,
      the variable v points to the positive atom [a] which wraps [f], while
      [a.neg] wraps the theory negation of [f]. *)

  and clause = {
    name : string;              (** Clause name, mainly for printing, unique. *)
    tag : int option;           (** User-provided tag for clauses. *)
    atoms : atom array;         (** The atoms that constitute the clause.*)
    mutable cpremise : premise; (** The premise of the clause, i.e. the justification
                                    of why the clause must be satisfied. *)
    mutable activity : float;   (** Clause activity, used for the heap heuristics. *)
    mutable attached : bool;    (** Is the clause attached, i.e. does it watch literals. *)
    mutable visited : bool;     (** Boolean used during propagation and proof generation. *)
  }
  (** The type of clauses. Each clause generated should be true, i.e. enforced
      by the current problem (for more information, see the cpremise field). *)

  and reason =
    | Decision        (** The atom has been decided by the sat solver *)
    | Bcp of clause   (** The atom has been propagated by the given clause *)
    | Semantic        (** The atom has been propagated by the theory at the given level. *)
  (** Reasons of propagation/decision of atoms. *)

  and premise =
    | Hyp                     (** The clause is a hypothesis, provided by the user. *)
    | Local                   (** The clause is a 1-atom clause,
                                  where the atom is a local assumption*)
    | Lemma of proof          (** The clause is a theory-provided tautology, with
                                  the given proof. *)
    | History of clause list  (** The clause can be obtained by resolution of the clauses
                                  in the list. If the list has a single element [c] , then
                                  the clause can be obtained by simplifying [c] (i.e
                                  eliminating doublons in its atom list).
                                  For a premise [History [a_1 :: ... :: a_n]] ([n > 0])
                                  the clause is obtained by performing resolution of
                                  [a_1] with [a_2], and then performing a resolution step between
                                  the result and [a_3], etc...
                                  Of course, each of the clause [a_i] also has its own premise. *)
  (** Premises for clauses. Indeed each clause generated during a run of the solver
      should be satisfied, the premise is the justification of why it should be
      satisfied by the solver. *)

  (** {2 Decisions and propagations} *)
  type t =
    | Lit of lit
    | Atom of atom (**)
  (** Either a lit of an atom *)

  val of_lit : lit -> t
  val of_atom : atom -> t
  (** Constructors and destructors *)

  (** {2 Elements} *)

  type elt =
    | E_lit of lit
    | E_var of var (**)
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
  val get_elt_idx : elt -> int
  val get_elt_weight : elt -> float
  val set_elt_level : elt -> int -> unit
  val set_elt_idx : elt -> int -> unit
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
  (** Returns the variable linked with the given formula, and whether the atom associated with the formula
      is [var.pa] or [var.na] *)

  val empty_clause : clause
  (** The empty clause *)
  val make_clause : ?tag:int -> string -> atom list -> premise -> clause
  (** [make_clause name atoms size premise] creates a clause with the given attributes. *)


  (** {2 Helpers} *)

  val mark : atom -> unit
  (** Mark the atom as seen, using the 'seen' field in the variable. *)

  val seen : atom -> bool
  (** Returns wether the atom has been marked as seen. *)

  val seen_both : var -> bool
  (** both atoms have been seen? *)

  val clear : var -> unit
  (** Clear the 'seen' field of the variable. *)


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

  val pp : Format.formatter -> t -> unit
  val pp_lit : Format.formatter -> lit -> unit
  val pp_atom : Format.formatter -> atom -> unit
  val pp_clause : Format.formatter -> clause -> unit
  val pp_dimacs : Format.formatter -> clause -> unit
  val pp_reason : Format.formatter -> (int * reason option) -> unit
  (** Debug function for atoms and clauses (very verbose) *)

end

