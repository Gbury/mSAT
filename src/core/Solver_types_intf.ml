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

type 'a printer = Format.formatter -> 'a -> unit

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
    name : int;                 (** Clause name, mainly for printing, unique. *)
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
  type trail_elt =
    | Lit of lit
    | Atom of atom (**)
  (** Either a lit of an atom *)

  (** {2 Elements} *)

  type elt =
    | E_lit of lit
    | E_var of var (**)
  (** Either a lit of a var *)

  val nb_elt : unit -> int
  val get_elt : int -> elt
  val iter_elt : (elt -> unit) -> unit
  (** Read access to the vector of variables created *)

  (** {2 Variables, Literals & Clauses } *)

  module Lit : sig
    type t = lit
    val term : t -> term
    val make : term -> t
    (** Returns the variable associated with the term *)

    val level : t -> int
    val set_level : t -> int -> unit

    val assigned : t -> term option
    val set_assigned : t -> term option -> unit
    val weight : t -> float
    val set_weight : t -> float -> unit

    val pp : t printer
    val debug : t printer
  end

  module Var : sig
    type t = var
    val dummy : t


    val pos : t -> atom
    val neg : t -> atom

    val level : t -> int
    val set_level : t -> int -> unit
    val reason : t -> reason option
    val set_reason : t -> reason option -> unit
    val assignable : t -> lit list option
    val set_assignable : t -> lit list option -> unit
    val weight : t -> float
    val set_weight : t -> float -> unit

    val make : formula -> t * Formula_intf.negated
    (** Returns the variable linked with the given formula,
        and whether the atom associated with the formula
        is [var.pa] or [var.na] *)

    val seen_both : t -> bool
    (** both atoms have been seen? *)

    val clear : t -> unit
    (** Clear the 'seen' field of the variable. *)
  end

  module Atom : sig
    type t = atom
    val dummy : t
    val level : t -> int
    val reason : t -> reason option
    val lit : t -> formula
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val var : t -> Var.t
    val abs : t -> t (** positive atom *)
    val neg : t -> t
    val id : t -> int
    val is_true : t -> bool
    val is_false : t -> bool

    val make : formula -> t
    (** Returns the atom associated with the given formula *)

    val mark : t -> unit
    (** Mark the atom as seen, using the 'seen' field in the variable. *)

    val seen : t -> bool
    (** Returns wether the atom has been marked as seen. *)

    val pp : t printer
    val pp_a : t array printer
    val debug : t printer
    val debug_a : t array printer
  end

  module Elt : sig
    type t = elt

    val of_lit : Lit.t -> t
    val of_var : Var.t -> t

    val id : t -> int
    val level : t -> int
    val idx : t -> int
    val weight : t -> float

    val set_level : t -> int -> unit
    val set_idx : t -> int -> unit
    val set_weight : t -> float -> unit
  end

  module Clause : sig
    type t = clause
    val dummy : t

    val name : t -> string
    val atoms : t -> Atom.t array
    val tag : t -> int option
    val premise : t -> premise
    val set_premise : t -> premise -> unit

    val visited : t -> bool
    val set_visited : t -> bool -> unit
    val attached : t -> bool
    val set_attached : t -> bool -> unit
    val activity : t -> float
    val set_activity : t -> float -> unit

    val empty : t
    (** The empty clause *)

    val make : ?tag:int -> Atom.t list -> premise -> clause
    (** [make_clause name atoms size premise] creates a clause with the given attributes. *)

    val pp : t printer
    val pp_dimacs : t printer
    val debug : t printer
  end

  module Trail_elt : sig
    type t = trail_elt

    val of_lit : Lit.t -> t
    val of_atom : Atom.t -> t
    (** Constructors and destructors *)
    val debug : t printer
  end
end

