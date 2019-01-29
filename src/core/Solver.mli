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
       and type lemma = Th.proof
       and type theory = Th.t

module Make_mcsat(Th : Solver_intf.PLUGIN_MCSAT)
  : S with module Term = Th.Term
       and module Formula = Th.Formula
       and type lemma = Th.proof
       and type theory = Th.t

module Make_pure_sat(Th: Solver_intf.PLUGIN_SAT)
  : S with type Term.t = Solver_intf.void
       and module Formula = Th.Formula
       and type lemma = Th.proof
       and type theory = unit


