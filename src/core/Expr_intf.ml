(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

(** Mcsat expressions

    This modules defines the required implementation of expressions for Mcsat.
*)

type negated = Formula_intf.negated =
  | Negated     (** changed sign *)
  | Same_sign   (** kept sign *)
(** This type is used during the normalisation of formulas.
    See {!val:Expr_intf.S.norm} for more details. *)

module type S = sig
  (** Signature of formulas that parametrises the Mcsat Solver Module. *)

  type proof
  (** An abstract type for proofs *)

  module Term : sig
    (** McSat Terms *)

    type t
    (** The type of terms *)

    val equal : t -> t -> bool
    (** Equality over terms. *)

    val hash : t -> int
    (** Hashing function for terms. Should be such that two terms equal according
        to {!val:Expr_intf.S.equal} have the same hash. *)

    val print : Format.formatter -> t -> unit
    (** Printing function used among other for debugging. *)

  end

  module Formula : sig
    (** McSat formulas *)

    type t
    (** The type of atomic formulas over terms. *)

    val equal : t -> t -> bool
    (** Equality over formulas. *)

    val hash : t -> int
    (** Hashing function for formulas. Should be such that two formulas equal according
        to {!val:Expr_intf.S.equal} have the same hash. *)

    val print : Format.formatter -> t -> unit
    (** Printing function used among other thing for debugging.  *)

    val dummy : t
    (** Constant formula. A valid formula should never be physically equal to [dummy] *)

    val neg : t -> t
    (** Formula negation *)

    val norm : t -> t * negated
    (** Returns a 'normalized' form of the formula, possibly negated
        (in which case return [Negated]).
        [norm] must be so that [a] and [neg a] normalise to the same formula,
        but one returns [Negated] and the other [Same_sign]. *)
  end


end

