
module type Arg = sig
  type proof
  type formula
  val prove : Format.formatter -> formula list -> unit
  val context : Format.formatter -> proof -> unit
  val translate : Format.formatter -> formula -> unit
end

module type S = sig
  type t
  val print : Format.formatter -> t -> unit
end


