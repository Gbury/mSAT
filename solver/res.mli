(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module type S = Res_intf.S

module Make :
  functor (L : Log_intf.S) ->
  functor (St : Solver_types.S) -> sig
    include S with module St = St
    val push : unit -> int
    val pop : int -> unit
  end
(** Functor to create a module building proofs from a sat-solver unsat trace. *)
