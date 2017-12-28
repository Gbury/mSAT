(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module Make(Dummy:sig end) =
  Msat_solver.Mcsolver.Make(struct
    type proof = unit
    module Term = Msat_smt.Expr_smt.Term
    module Formula = Msat_smt.Expr_smt.Atom
  end)(Plugin_mcsat)(struct end)

