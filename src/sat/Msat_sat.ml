(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2016 Guillaume Bury
*)

module Expr = Expr_sat

module F = Msat.Make_smt_expr(Expr)
include Msat.Make(F)(Msat.Make_dummy(F))

