(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module Make(Dummy : sig end) =
  Msat.Solver.Make
    (Msat.Expr.Atom)
    (Msat.Solver.DummyTheory
       (Msat.Expr.Atom))
    (struct end)

