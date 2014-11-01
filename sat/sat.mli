(* Copyright 2014 Guillaume Bury *)

module Make(Dummy: sig end) : sig
    type atom
    type state
    type res = Sat | Unsat

    val new_atom : unit -> atom
    val neg : atom -> atom

    val hash : atom -> int
    val equal : atom -> atom -> bool
    val compare : atom -> atom -> int

    val print_atom : Format.formatter -> atom -> unit
    val iter_atoms : (atom -> unit) -> unit

    val solve : unit -> res
    val eval : atom -> bool
    val assume : atom list list -> unit
end

