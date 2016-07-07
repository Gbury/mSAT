
module type S = Backend_intf.S

module type Arg = sig
  type atom
  type lemma

  val print_atom : Format.formatter -> atom -> unit
  val lemma_info : lemma -> string * string option * (Format.formatter -> unit -> unit) list
end

module Make(S : Res.S)(A : Arg with type atom := S.atom and type lemma := S.lemma) :
  S with type t := S.proof

