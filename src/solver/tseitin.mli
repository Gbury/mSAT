(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module type S = Tseitin_intf.S

module Make : functor
  (F : Tseitin_intf.Arg) -> S with type atom = F.t

