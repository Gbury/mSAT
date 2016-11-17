(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module Make(Dummy:sig end) =
  Mcsolver.Make(struct
    type proof = unit
    module Term = Expr_smt.Term
    module Formula = Expr_smt.Atom
  end)(Plugin_mcsat)(struct end)

