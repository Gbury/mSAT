(**************************************************************************)
(*                                                                        *)
(*                                  Cubicle                               *)
(*             Combining model checking algorithms and SMT solvers        *)
(*                                                                        *)
(*                  Sylvain Conchon, Evelyne Contejean                    *)
(*                  Francois Bobot, Mohamed Iguernelala, Alain Mebsout    *)
(*                  CNRS, Universite Paris-Sud 11                         *)
(*                                                                        *)
(*  Copyright 2011. This file is distributed under the terms of the       *)
(*  Apache Software License version 2.0                                   *)
(*                                                                        *)
(**************************************************************************)

module type S = sig
  (** Singature for theories to be given to the Solver. *)

  type t
  (** The type of states of the theory. Preferably not mutable. *)

  type formula
  (** The type of formulas. Should be compatble with Formula_intf.S *)

  type explanation
  (** The type of explanations. Should be compatible with
      Explanations.S.t with module St = Solver_types.S with type formula = fomula *)

  exception Inconsistent of explanation
  (** Exception raised by the theory when assuming an incoherent set of formulas. *)

  val dummy : t
  (** A dummy theory state. Should be physically different from any valid theory state. *)

  val empty : unit -> t
  (** A function to create an empty theory. *)

  val assume : cs:bool -> formula -> explanation -> t -> t
  (** Return a new theory state with the formula as assumption.
      @raise Inconsistent if the new state would be inconsistent. *)

end

