
(** Typechecking of terms from Dolmen.Term.t
    This module provides functions to parse terms from the untyped syntax tree
    defined in Dolmen, and generate formulas as defined in the Expr module. *)

exception Typing_error of string * Dolmen.Term.t

(** {2 High-level functions} *)

val new_decl : Dolmen.Id.t -> Dolmen.Term.t -> unit

val new_def  : Dolmen.Id.t -> Dolmen.Term.t -> unit

val new_formula : Dolmen.Term.t -> Msat.Expr.Formula.t

