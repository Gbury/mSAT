
module type S = sig
  type t
  val print : Format.formatter -> t -> unit
end

