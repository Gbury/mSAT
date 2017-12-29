(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

(** Resolution proofs

    This modules defines functions to create and manipulate resolution proofs.
*)

module type S = Res_intf.S
(** Interface for a module manipulating resolution proofs. *)

module type FULL = Res_intf.FULL

module Make : functor (St : Solver_types.S) -> FULL with module St = St
(** Functor to create a module building proofs from a sat-solver unsat trace. *)

