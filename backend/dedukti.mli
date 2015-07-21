
module type S = Backend_intf.S

module type Arg = sig
  type proof
  type formula
  val prove : Format.formatter -> formula list -> unit
  val context : Format.formatter -> proof -> unit
  val translate : Format.formatter -> formula -> unit
end

module Make :
  functor(S : Res.S) ->
  functor(A : Arg with type formula := S.atom and type proof := S.proof) ->
    S with type t := S.proof
(** Functor to generate a backend to output proofs for the dedukti type checker. *)
