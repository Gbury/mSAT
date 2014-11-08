(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module type S = Tseitin_intf.S

module Make : functor (F : Formula_intf.S) -> S with type atom = F.t
