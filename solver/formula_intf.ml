(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

type negated =
  | Negated (* changed sign *)
  | Same_sign (* kept sign *)

module type S = sig
  (** Signature of formulas that parametrises the SAT/SMT Solver Module. *)

  type t
  (** The type of atomic formulas. *)

  type proof
  (** An abstract type for proofs *)

  val dummy : t
  (** Formula constants. A valid formula should never be physically equal to [dummy] *)

  val fresh : unit -> t
  (** Returns a fresh literal, distinct from any other literal (used in cnf conversion) *)

  val neg : t -> t
  (** Formula negation *)

  val norm : t -> t * negated
  (** Returns a 'normalized' form of the formula, possibly negated
      (in which case return [Negated]).
      [norm] must be so that [a] and [neg a] normalise to the same formula,
      but one returns [Same_sign] and one returns [Negated] *)

  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  (** Usual hash and comparison functions. Given to Map and Hash functors. *)

  val print : Format.formatter -> t -> unit
  (** Printing function used for debugging. *)
end

