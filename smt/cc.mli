(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module Make(T : Sig.OrderedType) : sig
    type t

    exception Unsat of (T.t * T.t * (T.t list))

    val empty : t
    val add_eq : t -> T.t -> T.t -> t
    val add_neq : t -> T.t -> T.t -> t
end
