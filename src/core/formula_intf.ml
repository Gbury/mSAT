(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

(** SMT formulas

    This module defines the required implementation of formulas for
    an SMT solver.
*)

type negated =
  | Negated     (** changed sign *)
  | Same_sign   (** kept sign *)
(** This type is used during the normalisation of formulas.
    See {!val:Expr_intf.S.norm} for more details. *)

module type S = sig
  (** SMT formulas *)

  type t
  (** The type of atomic formulas. *)

  type proof
  (** An abstract type for proofs *)

  val equal : t -> t -> bool
  (** Equality over formulas. *)

  val hash : t -> int
  (** Hashing function for formulas. Should be such that two formulas equal according
      to {!val:Expr_intf.S.equal} have the same hash. *)

  val print : Format.formatter -> t -> unit
  (** Printing function used among other thing for debugging.  *)

  val dummy : t
  (** Formula constant. A valid formula should never be physically equal to [dummy] *)

  val neg : t -> t
  (** Formula negation. Should be an involution, i.e. [equal a (neg neg a)] should
      always hold. *)

  val norm : t -> t * negated
  (** Returns a 'normalized' form of the formula, possibly negated
      (in which case return [Negated]). This function is used to recognize
      the link between a formula [a] and its negation [neg a], so the goal is
      that [a] and [neg a] normalise to the same formula,
      but one returns [Same_sign] and the other one returns [Negated] *)

end

