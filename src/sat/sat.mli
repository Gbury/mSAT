(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

open Msat

module Make(Dummy : sig end) : Solver.S with type St.formula = Expr.Atom.t


