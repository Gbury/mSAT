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

type ('formula, 'proof) res = ('formula, 'proof) Plugin_intf.res =
  | Sat
  | Unsat of 'formula list * 'proof
(** Type returned by the theory, either the current set of assumptions is satisfiable,
    or it is not, in which case a tautological clause (hopefully minimal) is returned.
    Formulas in the unsat clause must come from the current set of assumptions, i.e
    must have been encountered in a slice. *)

type ('form, 'proof) slice = {
  start : int;
  length : int;
  get : int -> 'form;
  push : 'form list -> 'proof -> unit;
  propagate : 'form -> 'form list -> 'proof -> unit;
}
(** The type for a slice of literals to assume/propagate in the theory.
    [get] operations should only be used for integers [ start <= i < start + length].
    [push clause proof] allows to add a tautological clause to the sat solver.
    [propagate lit causes proof] informs the solver to propagate [lit], with the reason
    that the clause [causes => lit] is a theory tautology. It is faster than pushing
    the associated clause but the clause will not be remembered by the sat solver,
    i.e it will not be used by the solver to do boolean propagation. *)

module type S = sig
  (** Signature for theories to be given to the Solver. *)

  type formula
  (** The type of formulas. Should be compatble with Formula_intf.S *)

  type proof
  (** A custom type for the proofs of lemmas produced by the theory. *)

  type level
  (** The type for levels to allow backtracking. *)

  val dummy : level
  (** A dummy level. *)

  val current_level : unit -> level
  (** Return the current level of the theory (either the empty/beginning state, or the
      last level returned by the [assume] function). *)

  val assume : (formula, proof) slice -> (formula, proof) res
  (** Assume the formulas in the slice, possibly pushing new formulas to be propagated,
      and returns the result of the new assumptions. *)

  val backtrack : level -> unit
  (** Backtrack to the given level. After a call to [backtrack l], the theory should be in the
      same state as when it returned the value [l], *)

  val if_sat : (formula, proof) slice -> unit
  (** Called at the end of the search in case a model has been found. If no new clause is
      pushed, then 'sat' is returned, else search is resumed. *)

end

