(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2015 Guillaume Bury
*)

(** Coq Backend

    This module provides an easy way to produce coq scripts
    corresponding to the resolution proofs output by the
    sat solver. *)
open Msat

module type S = Backend_intf.S
(** Interface for exporting proofs. *)

module type Arg = sig
  (** Term printing for Coq *)

  type hyp
  type lemma
  type assumption
  (** The types of hypotheses, lemmas, and assumptions *)

  val prove_hyp : Format.formatter -> string -> hyp -> unit
  val prove_lemma : Format.formatter -> string -> lemma -> unit
  val prove_assumption : Format.formatter -> string -> assumption -> unit
  (** Proving function for hypotheses, lemmas and assumptions.
      [prove_x fmt name x] should prove [x], and be such that after
      executing it, [x] is among the coq hypotheses under the name [name].
      The hypothesis should be the encoding of the given clause, i.e
      for a clause [a \/ not b \/ c], the proved hypothesis should be:
      [ ~ a -> ~ ~ b -> ~ c -> False ], keeping the same order as the
      one in the atoms array of the clause. *)

end

module Make(S : Res.S)(A : Arg with type hyp := S.clause
                                and type lemma := S.clause
                                and type assumption := S.clause) : S with type t := S.proof
(** Base functor to output Coq proofs *)


module Simple(S : Res.S)(A : Arg with type hyp = S.St.formula list
                                  and type lemma := S.lemma
                                  and type assumption := S.St.formula) : S with type t := S.proof
(** Simple functo to output Coq proofs *)

