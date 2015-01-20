(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module Make (L : Log_intf.S)(E : Expr_intf.S)
    (Th : Plugin_intf.S with type term = E.Term.t and type formula = E.Formula.t) : sig
  (** Functor to create a  McSolver parametrised by the atomic formulas and a theory. *)

  exception Unsat

  module St : Mcsolver_types.S
    with type term = E.Term.t
     and type formula = E.Formula.t

  module Proof : Res.S
    with type atom = St.atom
     and type clause = St.clause
     and type lemma = Th.proof

  val solve : unit -> unit
  (** Try and solves the current set of assumptions.
      @return () if the current set of clauses is satisfiable
      @raise Unsat if a toplevel conflict is found *)

  val assume : E.Formula.t list list -> cnumber:int -> unit
  (** Add the list of clauses to the current set of assumptions.
      Modifies the sat solver state in place.
      @raise Unsat if a conflict is detect when adding the clauses *)

  val eval : E.Formula.t -> bool
  (** Returns the valuation of a formula in the current state
      of the sat solver. *)

  val history : unit -> St.clause Vec.t
  (** Returns the history of learnt clauses, in the right order. *)

  val unsat_conflict : unit -> St.clause option
  (** Returns the unsat clause found at the toplevel, if it exists (i.e if
      [solve] has raised [Unsat]) *)

  val model : unit -> (St.term * St.term) list
  (** Returns the model found if the formula is satisfiable. *)

  type level
  (** Abstract notion of assumption level. *)

  val base_level : level
  (** Level with no assumption at all, corresponding to the empty solver *)

  val current_level : unit -> level
  (** The current level *)

  val push : unit -> level
  (** Create a new level that extends the previous one. *)

  val pop : level -> unit
  (** Go back to the given level, forgetting every assumption added since.
      @raise Invalid_argument if the current level is below the argument *)

  val clear : unit -> unit
  (** Return to level {!base_level} *)
end

