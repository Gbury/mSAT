(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module type S = Res_intf.S

module Make : functor (St : Solver_types.S)(Proof : sig type proof end)
  -> S with type atom= St.atom and type clause = St.clause and type lemma = Proof.proof
