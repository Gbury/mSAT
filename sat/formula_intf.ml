(**************************************************************************)
(*                                                                        *)
(*                                  Cubicle                               *)
(*             Combining model checking algorithms and SMT solvers        *)
(*                                                                        *)
(*                  Guillaume Bury                                        *)
(*                  INRIA                                                 *)
(*                  Sylvain Conchon and Alain Mebsout                     *)
(*                  Universite Paris-Sud 11                               *)
(*                                                                        *)
(*  Copyright 2011. This file is distributed under the terms of the       *)
(*  Apache Software License version 2.0                                   *)
(*                                                                        *)
(**************************************************************************)

module type S = sig
  (** Signature of formulas that parametrises the SMT Solver Module. *)

  type t
  (** The type of atomic formulas. *)

  val dummy : t
  (** Formula constants. A valid formula should never be physically equal to [dummy] *)

  val neg : t -> t
  (** Formula negation *)

  val norm : t -> t * bool
  (** Returns a 'normalized' form of the formula, possibly negated (in which case return true).
      [norm] must be so that [a] and [neg a] normalises to the same formula. *)

  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  (** Usual hash and comparison functions. Given to Map and Hash functors. *)

  val label : t -> Hstring.t
  val add_label : Hstring.t -> t -> unit
  (** Optional. Not yet used in Solver. *)

  val print : Format.formatter -> t -> unit
  (** Printing function used for debugging. *)
end

