(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2016 Guillaume Bury
*)

module Expr = Expr_sat

module Make(Dummy : sig end) =
  Solver.Make(Expr)(Solver.DummyTheory(Expr))(struct end)

