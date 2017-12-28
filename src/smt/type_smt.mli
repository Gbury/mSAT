
(** Typechecking of terms from Dolmen.Term.t
    This module provides functions to parse terms from the untyped syntax tree
    defined in Dolmen, and generate formulas as defined in the Expr_smt module. *)

include Msat_solver.Type.S with type atom := Expr_smt.Atom.t

