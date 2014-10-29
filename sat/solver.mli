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

module Make (F : Formula_intf.S)(Th : Theory_intf.S with type formula = F.t) : sig

    module St : Solver_types.S with type formula = F.t

    exception Sat
    exception Unsat of St.clause list

    type state

    val solve : unit -> unit
    val assume : F.t list list -> cnumber : int -> unit
    val clear : unit -> unit

    val eval : F.t -> bool
    val save : unit -> state
    val restore : state -> unit

end
