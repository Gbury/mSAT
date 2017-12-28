(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

(** Typechecking of terms from Dolmen.Term.t
    This module provides functions to parse terms from the untyped syntax tree
    defined in Dolmen, and generate formulas as defined in the Expr_sat module. *)

include Msat_solver.Type.S with type atom := Expr_sat.t

