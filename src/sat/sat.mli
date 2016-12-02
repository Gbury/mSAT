(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2016 Guillaume Bury
*)

(** Sat solver

    This modules instanciates a pure sat solver using integers to represent
    atomic propositions.
*)

module Expr : sig
  (** SAT Formulas

      This modules implements formuals adequate for use in a pure SAT Solver.
      Atomic formuals are represented using integers, that should allow
      near optimal efficiency (both in terms of space and time).
  *)

  include Formula_intf.S
  (** This modules implements the requirements for implementing an SAT Solver. *)

  val make : int -> t
  (** Make a proposition from an integer. *)

  val fresh : unit -> t
  (** Make a fresh atom *)

end
(** The module defining formulas *)

module Make(Dummy : sig end) : Solver.S with type St.formula = Expr.t
(** A functor that can generate as many solvers as needed. *)

