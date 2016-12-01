(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2016 Guillaume Bury
*)

(** Sat solver

    This modules instanciates a pure sat solver using integers to represent
    atomic propositions.
*)

module Expr = Expr_sat
(** The module defining formulas *)

module Make(Dummy : sig end) : Solver.S with type St.formula = Expr_sat.t
(** A functor that can generate as many solvers as needed. *)

