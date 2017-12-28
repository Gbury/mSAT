(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module Make() =
  Msat_solver.Mcsolver.Make(struct
    type proof = unit
    module Term = Msat_smt.Expr.Term
    module Formula = Msat_smt.Expr.Atom
  end)(Plugin_mcsat)()

