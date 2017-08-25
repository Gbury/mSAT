(**************************************************************************)
(*                                                                        *)
(*                                  Cubicle                               *)
(*             Combining model checking algorithms and SMT solvers        *)
(*                                                                        *)
(*                  Sylvain Conchon, Evelyne Contejean                    *)
(*                  Francois Bobot, Mohamed Iguernelala, Alain Mebsout    *)
(*                  CNRS, Universite Paris-Sud 11                         *)
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

(** McSat Theory

    This module defines what a theory must implement in order to
    be sued in a McSat solver.
*)

type 'term eval_res =
  | Unknown                       (** The given formula does not have an evaluation *)
  | Valued of bool * ('term list) (** The given formula can be evaluated to the given bool.
                                      The list of terms to give is the list of terms that
                                      were effectively used for the evaluation. *)
(** The type of evaluation results for a given formula.
    For instance, let's suppose we want to evaluate the formula [x * y = 0], the
    following result are correct:
    - [Unknown] if neither [x] nor [y] are assigned to a value
    - [Valued (true, [x])] if [x] is assigned to [0]
    - [Valued (true, [y])] if [y] is assigned to [0]
    - [Valued (false, [x; y])] if [x] and [y] are assigned to 1 (or any non-zero number)
*)

type ('formula, 'proof) res =
  | Sat
  (** The current set of assumptions is satisfiable. *)
  | Unsat of 'formula list * 'proof
  (** The current set of assumptions is *NOT* satisfiable, and here is a
      theory tautology (with its proof), for which every litteral is false
      under the current assumptions. *)
(** Type returned by the theory. Formulas in the unsat clause must come from the
    current set of assumptions, i.e must have been encountered in a slice. *)

type ('term, 'formula) assumption =
  | Lit of 'formula         (** The given formula is asserted true by the solver *)
  | Assign of 'term * 'term (** The first term is assigned to the second *)
(** Asusmptions made by the core SAT solver. *)

type ('term, 'formula, 'proof) reason =
  | Eval of 'term list                      (** The formula can be evalutaed using the terms in the list *)
  | Consequence of 'formula list * 'proof   (** [Consequence (l, p)] means that the formulas in [l] imply
                                                the propagated formula [f]. The proof should be a proof of
                                                the clause "[l] implies [f]". *)
(** The type of reasons for propagations of a formula [f]. *)

type ('term, 'formula, 'proof) slice = {
  start : int;                                (** Start of the slice *)
  length : int;                               (** Length of the slice *)
  get : int -> ('term, 'formula) assumption;  (** Accessor for the assertions in the slice.
                                                  Should only be called on integers [i] s.t.
                                                  [start <= i < start + length] *)
  push : 'formula list -> 'proof -> unit;     (** Add a clause to the solver. *)
  propagate : 'formula ->
    ('term, 'formula, 'proof) reason -> unit; (** Propagate a formula, i.e. the theory can
                                                  evaluate the formula to be true (see the
                                                  definition of {!type:eval_res} *)
}
(** The type for a slice of assertions to assume/propagate in the theory. *)

module type S = sig
  (** Signature for theories to be given to the Model Constructing Solver. *)

  type term
  (** The type of terms. Should be compatible with Expr_intf.Term.t*)

  type formula
  (** The type of formulas. Should be compatble with Expr_intf.Formula.t *)

  type proof
  (** A custom type for the proofs of lemmas produced by the theory. *)

  type level
  (** The type for levels to allow backtracking. *)

  val dummy : level
  (** A dummy level. *)

  val current_level : unit -> level
  (** Return the current level of the theory (either the empty/beginning state, or the
      last level returned by the [assume] function). *)

  val assume : (term, formula, proof) slice -> (formula, proof) res
  (** Assume the formulas in the slice, possibly pushing new formulas to be propagated,
      and returns the result of the new assumptions. *)

  val if_sat : (term, formula, proof) slice -> (formula, proof) res
  (** Called at the end of the search in case a model has been found. If no new clause is
      pushed and the function returns [Sat], then proof search ends and 'sat' is returned,
      else search is resumed. *)

  val backtrack : level -> unit
  (** Backtrack to the given level. After a call to [backtrack l], the theory should be in the
      same state as when it returned the value [l], *)

  val assign : term -> term
  (** Returns an assignment value for the given term. *)

  val iter_assignable : (term -> unit) -> formula -> unit
  (** An iterator over the subterms of a formula that should be assigned a value (usually the poure subterms) *)

  val eval : formula -> term eval_res
  (** Returns the evaluation of the formula in the current assignment *)

end

