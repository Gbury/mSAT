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

  type proof
  (** An abstract type for proofs *)

  module Term : sig

    type t
    (** The type of terms *)

    val hash : t -> int
    val equal : t -> t -> bool
    val print : Format.formatter -> t -> unit
    (** Common functions *)

  end

  module Formula : sig

    type t
    (** The type of atomic formulas over terms. *)

    val hash : t -> int
    val equal : t -> t -> bool
    val print : Format.formatter -> t -> unit
    (** Common functions *)

    val dummy : t
    (** Formula constants. A valid formula should never be physically equal to [dummy] *)

    val neg : t -> t
    (** Formula negation *)

    val norm : t -> t * negated
    (** Returns a 'normalized' form of the formula, possibly negated
        (in which case return [Negated]).
        [norm] must be so that [a] and [neg a] normalise to the same formula,
        but one returns [Negated] and the other [Same_sign]. *)
  end


end

