(* Copyright 2014 Guillaume Bury *)

module type S = sig
    type clause
    type lemma

    val proven : clause -> bool

    val add_assumption : clause -> unit
    val add_th_lemma : clause -> lemma -> unit
    val add_clause : clause -> clause list -> unit

end
