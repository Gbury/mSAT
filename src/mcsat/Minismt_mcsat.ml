(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

include
  Minismt.Mcsolver.Make(struct
    type proof = unit
    module Term = Minismt_smt.Expr.Term
    module Formula = Minismt_smt.Expr.Atom
  end)(Plugin_mcsat)

