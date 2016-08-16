(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)
open Msat

type term = Term.t

type formula = private
  | Prop of int (* prop or tseitin atom *)
  | Equal of term * term
  | Distinct of term * term

type t = formula
type proof = unit

include Formula_intf.S with type t := formula and type proof := proof

val dummy : t

val fresh : unit -> t

val mk_prop : int -> t
(** [mk_prop i] makes a prop literal from [i], whose sign matters.
    @raise Invalid_prop if [i=0] or if [i] is too large *)

val mk_true : t
val mk_false : t
val mk_atom : term -> t
val mk_atom_neg : term -> t
val mk_eq : term -> term -> t
val mk_neq : term -> term -> t

module Term = Term
module Formula : sig
  type t = formula
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val print : Format.formatter -> t -> unit
end
