(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module Th = Solver.DummyTheory(Expr_smt.Atom)

module Make(Dummy:sig end) =
  Solver.Make(Expr_smt.Atom)(Th)(struct end)

