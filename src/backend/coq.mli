(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2015 Guillaume Bury
*)

(** Coq Backend

    This module provides an easy way to produce coq scripts
    corresponding to the resolution proofs output by the
    sat solver. *)

module type S = Backend_intf.S
(** Interface for exporting proofs. *)

module type Arg = sig
  (** Term printing for Coq *)

  type atom
  (** The type of atomic formulas *)

  type hyp
  type lemma
  type assumption
  (** The types of hypotheses, lemmas, and assumptions *)

  val print_atom : Format.formatter -> atom -> unit
  (** Print the given atomic formula *)

  val prove_hyp : Format.formatter -> string -> hyp -> unit
  val prove_lemma : Format.formatter -> string -> lemma -> unit
  val prove_assumption : Format.formatter -> string -> assumption -> unit
  (** Proving function for hypotheses, lemmas and assumptions.
      [prove_x fmt name x] should prove [x], and be such that after
      executing it, [x] is among the coq hypotheses under the name [name]. *)

end

module Make(S : Res.S)(A : Arg with type atom := S.atom
                                and type hyp := S.clause
                                and type lemma := S.clause
                                and type assumption := S.clause) : S with type t := S.proof
(** Base functor to output Coq proofs *)


module Simple(S : Res.S)(A : Arg with type atom := S.St.formula
                                  and type hyp = S.St.formula list
                                  and type lemma := S.lemma
                                  and type assumption := S.St.formula) : S with type t := S.proof
(** Simple functo to output Coq proofs *)

