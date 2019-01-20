(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

(** mSAT safe interface

    This module defines a safe interface for the core solver.
    It is the basis of the {!module:Solver} and {!module:Mcsolver} modules.
*)

module type S = Solver_intf.S
(** Safe external interface of solvers. *)

module Make_cdcl_t(Th : Solver_intf.PLUGIN_CDCL_T)
  : S with type Term.t = Solver_intf.void
       and module Formula = Th.Formula
       and type lemma = Th.lemma
       and type theory = Th.t
       and type proof = Th.lemma Th.Proof.t

module Make_mcsat(Th : Solver_intf.PLUGIN_MCSAT)
  : S with module Term = Th.Term
       and module Formula = Th.Formula
       and type lemma = Th.lemma
       and type theory = Th.t
       and type proof = Th.lemma Th.Proof.t

module Make_pure_sat(F: Solver_intf.PLUGIN_SAT)
  : S with type Term.t = Solver_intf.void
       and module Formula = F.Formula
       and type lemma = Solver_intf.void
       and type theory = unit
       and type proof = Solver_intf.void F.Proof.t

module Proof_empty : Solver_intf.PROOF with type _ t = unit
(** Empty proof manager, doesn't log anything *)
