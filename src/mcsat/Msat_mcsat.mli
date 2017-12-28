(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module Make() : Msat_solver.Solver.S with type St.formula = Msat_smt.Expr.atom

