(**************************************************************************)
(*                                                                        *)
(*                          Alt-Ergo Zero                                 *)
(*                                                                        *)
(*                  Sylvain Conchon and Alain Mebsout                     *)
(*                      Universite Paris-Sud 11                           *)
(*                                                                        *)
(*  Copyright 2011. This file is distributed under the terms of the       *)
(*  Apache Software License version 2.0                                   *)
(*                                                                        *)
(**************************************************************************)

(** Interfaces for Tseitin's CNF conversion *)

module type Arg = sig
  (** Formulas

      This defines what is needed of formulas in order to implement
      Tseitin's CNF conversion.
  *)

  type t
  (** Type of atomic formulas. *)

  val neg : t -> t
  (** Negation of atomic formulas. *)

  val fresh : unit -> t
  (** Generate fresh formulas (that are different from any other). *)

  val print : Format.formatter -> t -> unit
  (** Print the given formula. *)

end

module type S = sig
  (** CNF conversion

      This modules allows to convert arbitrary boolean formulas
      into CNF.
  *)

  type atom
  (** The type of atomic formulas. *)

  type t
  (** The type of arbitrary boolean formulas. Arbitrary boolean formulas
      can be built using functions in this module, and then converted
      to a CNF, which is a list of clauses that only use atomic formulas. *)

  val f_true : t
  (** The [true] formula, i.e a formula that is always satisfied. *)

  val f_false : t
  (** The [false] formula, i.e a formula that cannot be satisfied. *)

  val make_atom : atom -> t
  (** [make_atom p] builds the boolean formula equivalent to the atomic formula [p]. *)

  val make_not : t -> t
  (** Creates the negation of a boolean formula. *)

  val make_and : t list -> t
  (** Creates the conjunction of a list of formulas. An empty conjunction is always satisfied. *)

  val make_or : t list -> t
  (** Creates the disjunction of a list of formulas. An empty disjunction is never satisfied. *)

  val make_xor : t -> t -> t
  (** [make_xor p q] creates the boolean formula "[p] xor [q]". *)

  val make_imply : t -> t -> t
  (** [make_imply p q] creates the boolean formula "[p] implies [q]". *)

  val make_equiv : t -> t -> t
  (** [make_equiv p q] creates the boolena formula "[p] is equivalent to [q]". *)

  val make_cnf : t -> atom list list
  (** [make_cnf f] returns a conjunctive normal form of [f] under the form: a
      list (which is a conjunction) of lists (which are disjunctions) of
      atomic formulas. *)

  val print : Format.formatter -> t -> unit
  (** [print fmt f] prints the formula on the formatter [fmt].*)

end
