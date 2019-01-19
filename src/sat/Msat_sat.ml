(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2016 Guillaume Bury
*)

module Expr = Expr_sat
include Msat.Make_pure_sat(Expr)

