(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module Expr = Expr_smt
module Type = Type_smt

module Th = Msat_solver.Solver.DummyTheory(Expr_smt.Atom)

module Make() =
  Msat_solver.Solver.Make(Expr_smt.Atom)(Th)()

