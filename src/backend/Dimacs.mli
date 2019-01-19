(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

(** Dimacs backend for problems

    This module provides functiosn to export problems to the dimacs and
    iCNF formats.
*)

open Msat

module type ARG = sig
  type clause
  val lits : clause -> int list
end

module type S = sig
  type st

  type clause
  (** The type of clauses *)

  val export :
    st ->
    Format.formatter ->
    hyps:clause Vec.t ->
    history:clause Vec.t ->
    local:clause Vec.t ->
    unit
  (** Export the given clause vectors to the dimacs format.
      The arguments should be transmitted directly from the corresponding
      function of the {Internal} module. *)

  val export_icnf :
    Format.formatter ->
    hyps:clause Vec.t ->
    history:clause Vec.t ->
    local:clause Vec.t ->
    unit
  (** Export the given clause vectors to the dimacs format.
      The arguments should be transmitted directly from the corresponding
      function of the {Internal} module.
      This function may be called multiple times in order to add
      new clauses (and new local hyps) to the problem.
  *)

end

module Make(St: Msat.S)(A: ARG with type clause = St.clause)
  : S with type clause := St.clause and type st = St.t
(** Functor to create a module for exporting probems to the dimacs (& iCNF) formats. *)

