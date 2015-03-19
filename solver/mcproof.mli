(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module type S = Res_intf.S

module Make :
  functor (L : Log_intf.S) ->
  functor (St : Mcsolver_types.S) ->
    S with type atom = St.atom and type clause = St.clause and type lemma = St.proof
(** Functor to create a module building proofs from a sat-solver unsat trace. *)
