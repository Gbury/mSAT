(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

(** Create SAT/SMT Solvers

    This module provides a functor to create an SMT solver. Additionally, it also
    gives a functor that produce an adequate empty theory that can be given to the [Make]
    functor in order to create a pure SAT solver.
*)

module type S = Msat.S
(** The interface of instantiated solvers. *)

module DummyTheory(F : Formula_intf.S) :
  Theory_intf.S with type formula = F.t
                 and type proof = F.proof
(** Simple case where the theory is empty and
    the proof type is the one given in the formula interface *)

module Make (F : Formula_intf.S)
    (Th : Theory_intf.S with type formula = F.t
                         and type proof = F.proof)
  : S with type St.formula = F.t
       and type St.proof = F.proof
(** Functor to create a SMT Solver parametrised by the atomic
    formulas and a theory. *)

