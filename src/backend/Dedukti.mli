(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

(** Deduki backend for proofs

    Work in progress...
*)

open Msat

module type S = Backend_intf.S

module type Arg = sig

  type lemma
  type proof
  type formula

  val print : Format.formatter -> formula -> unit
  val prove : Format.formatter -> lemma -> unit
  val context : Format.formatter -> proof -> unit
end

module Make :
  functor(S : Res.S) ->
  functor(A : Arg
          with type formula := S.formula
           and type lemma := S.lemma
           and type proof := S.proof) ->
    S with type t := S.proof
(** Functor to generate a backend to output proofs for the dedukti type checker. *)
