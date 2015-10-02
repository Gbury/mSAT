
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
  functor(A : Arg with type formula := S.St.formula and type proof := S.proof) ->
    S with type t := S.proof
(** Functor to generate a backend to output proofs for the dedukti type checker. *)
