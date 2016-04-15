(**************************************************************************)
(*                                                                        *)
(*                                  Cubicle                               *)
(*             Combining model checking algorithms and SMT solvers        *)
(*                                                                        *)
(*                  Mohamed Iguernelala                                   *)
(*                  Universite Paris-Sud 11                               *)
(*                                                                        *)
(*  Copyright 2011. This file is distributed under the terms of the       *)
(*  Apache Software License version 2.0                                   *)
(*                                                                        *)
(**************************************************************************)

module type S = Solver_intf.S

(** Simple case where the proof type is [unit] and the theory is empty *)
module DummyTheory(F : Formula_intf.S) :
  Theory_intf.S with type formula = F.t and type proof = F.proof

module Make (F : Formula_intf.S)
    (Th : Theory_intf.S with type formula = F.t and type proof = F.proof)
    (Dummy: sig end) :
  S with type St.formula = F.t
     and type St.proof = F.proof
  (** Functor to create a SMT Solver parametrised by the atomic
      formulas and a theory. *)

