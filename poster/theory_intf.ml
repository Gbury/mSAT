
type ('formula, 'proof) res =
  | Sat | Unsat of 'formula list * 'proof

type ('form, 'proof) slice = {
  start : int;
  length : int;
  get : int -> 'form;
  push : 'form list -> 'proof -> unit;
}

module type S = sig
  val dummy : level
  val backtrack : level -> unit
  val current_level : unit -> level
  val assume : (formula, proof) slice
    -> (formula, proof) res
  val if_sat : (formula, proof) slice
    -> (formula, proof) res
end
