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

exception Sat
exception Unsat of Solver_types.clause list

module Make (Dummy : sig end) : sig
  type state

  val solve : unit -> unit
  val assume : Literal.LT.t list list -> cnumber : int -> unit
  val clear : unit -> unit

  val save : unit -> state
  val restore : state -> unit

end
