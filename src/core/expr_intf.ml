(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

type negated = Formula_intf.negated =
  | Negated (* changed sign *)
  | Same_sign (* kept sign *)

module type S = sig
  (** Signature of formulas that parametrises the Mcsat Solver Module. *)

  module Term : sig
    (** The type of terms *)
    type t
    val hash : t -> int
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val print : Format.formatter -> t -> unit
  end

  module Formula : sig
    (** The type of atomic formulas over terms. *)
    type t
    val hash : t -> int
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val print : Format.formatter -> t -> unit
  end

  type proof
  (** An abstract type for proofs *)

  val dummy : Formula.t
  (** Formula constants. A valid formula should never be physically equal to [dummy] *)

  val fresh : unit -> Formula.t
  (** Returns a fresh litteral, distinct from any other literal (used in cnf conversion) *)

  val neg : Formula.t -> Formula.t
  (** Formula negation *)

  val norm : Formula.t -> Formula.t * negated
  (** Returns a 'normalized' form of the formula, possibly negated
      (in which case return [Negated]).
      [norm] must be so that [a] and [neg a] normalise to the same formula,
      but one returns [Negated] and the other [Same_sign]. *)

end

