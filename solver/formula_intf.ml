(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module type S = sig
  (** Signature of formulas that parametrises the SAT/SMT Solver Module. *)

  type t
  (** The type of atomic formulas. *)

  type proof
  (** An abstract type for proofs *)

  val dummy : t
  (** Formula constants. A valid formula should never be physically equal to [dummy] *)

  val fresh : unit -> t
  (** Returns a fresh litteral, distinct from any other literal (used in cnf conversion) *)

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

