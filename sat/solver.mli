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

module Make (F : Formula_intf.S)
    (St : Solver_types.S with type formula = F.t)
    (Ex : Explanation.S with type atom = St.atom)
    (Th : Theory_intf.S with type formula = F.t and type explanation = Ex.t) : sig
  (** Functor to create a SMT Solver parametrised by the atomic formulas and a theory. *)

  exception Sat
  exception Unsat of St.clause list
  (** Exceptions raised by the [solve] function to return the nature of the current set of assummtions.
      Once the [Unsat] exception is raised, the solver needs to be cleared before anything else is done. *)

  type t
  (** The type of the state of the sat solver. Mutable.*)

  val solve : unit -> unit
  (** Try and solves the current set of assumptions.
      @raise Sat if the current set of assummptions is satisfiable.
      @raise Unsat if the current set of assumptions is unsatisfiable *)

  val assume : F.t list list -> cnumber : int -> unit
  (** Add the list of clauses to the current set of assumptions. Modifies the sat solver state in place. *)

  val clear : unit -> unit
  (** Resets everything done. Basically returns the solver to a state similar to when the module was created. *)

  val eval : F.t -> bool
  (** Returns the valuation of a formula in the current state of the sat solver. *)

  val save : unit -> t
  val restore : t -> unit
  (** Functions to be replaced by push&pop functions. *)

end

