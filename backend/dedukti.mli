
module type S = Backend_intf.S

module Make :
  functor(S : Res.S) ->
  functor(A : Backend_intf.Arg with type formula := S.atom and type proof := S.proof) ->
    S with type t := S.proof
(** Functor to generate a backend to output proofs for the dedukti type checker. *)
