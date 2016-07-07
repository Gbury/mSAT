(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module type S = Solver_intf.S

(** Simple case where the proof type is [unit] and the theory is empty *)
module DummyTheory(F : Formula_intf.S) :
  Theory_intf.S with type formula = F.t
                 and type proof = F.proof

module Make (F : Formula_intf.S)
    (Th : Theory_intf.S with type formula = F.t
                         and type proof = F.proof)
    (Dummy: sig end) :
  S with type St.formula = F.t
     and type St.proof = F.proof
  (** Functor to create a SMT Solver parametrised by the atomic
      formulas and a theory. *)

