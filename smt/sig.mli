(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)


module type OrderedType = sig
    (** Signature for ordered types (mainly for use in Map.Make) *)

    type t
    val compare : t -> t -> int
end
