(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module Make() : Minismt.Solver.S with type St.formula = Minismt_smt.Expr.atom

