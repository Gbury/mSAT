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

module type Arg = sig

  type t
  (** Type of atomic formulas *)

  val neg : t -> t
  (** Negation of atomic formulas *)

  val fresh : unit -> t
  (** Generate fresh formulas *)

  val print : Format.formatter -> t -> unit
  (** Print the given formula *)

end

module type S = sig

  (** The type of ground formulas *)
  type t
  type atom

  val f_true : t
  val f_false : t

  val make_atom : atom -> t
  (** [make_pred p] builds the atomic formula [p = true].
      @param sign the polarity of the atomic formula *)

  val make_not : t -> t
  val make_and : t list -> t
  val make_or : t list -> t
  val make_imply : t -> t -> t
  val make_equiv : t -> t -> t
  val make_xor : t -> t -> t

  val make_cnf : t -> atom list list
  (** [make_cnf f] returns a conjunctive normal form of [f] under the form: a
      list (which is a conjunction) of lists (which are disjunctions) of
      literals. *)

  val print : Format.formatter -> t -> unit
  (** [print fmt f] prints the formula on the formatter [fmt].*)

end
