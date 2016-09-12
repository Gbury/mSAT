(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module Make(Dummy : sig end) =
  Solver.Make(Expr_sat)(Solver.DummyTheory(Expr_sat))(struct end)

