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

module type S = sig

  type clause
  (** The type of clauses *)

  val export :
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

module Make(St: Solver_types_intf.S)(Dummy: sig end) : S with type clause := St.clause
(** Functor to create a module for exporting probems to the dimacs (& iCNF) formats. *)

