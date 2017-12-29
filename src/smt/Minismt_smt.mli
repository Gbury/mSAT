(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module Expr = Expr_smt
module Type = Type_smt

include Minismt.Solver.S with type St.formula = Expr_smt.atom

