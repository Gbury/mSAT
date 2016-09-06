(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module Make
  (St : Solver_types.S)
  (Th : Plugin_intf.S with type term = St.term and type formula = St.formula and type proof = St.proof)
  (Dummy: sig end)
: sig
  (** Functor to create a solver parametrised by the atomic formulas and a theory. *)

  (** {2 Solving facilities} *)

  exception Unsat
  exception UndecidedLit

  val solve : unit -> unit
  (** Try and solves the current set of assumptions.
      @return () if the current set of clauses is satisfiable
      @raise Unsat if a toplevel conflict is found *)

  val assume : ?tag:int -> St.formula list list -> unit
  (** Add the list of clauses to the current set of assumptions.
      Modifies the sat solver state in place. *)

  val push : unit -> unit
  (** Create a decision level for local assumptions.
      @raise Unsat if a conflict is detected in the current state. *)

  val pop : unit -> unit
  (** Pop a decision level for local assumptions. *)

  val local : St.formula list -> unit
  (** Add local assumptions
      @param assumptions list of additional local assumptions to make,
        removed after the callback returns a value *)

  val eval : St.formula -> bool
  (** Returns the valuation of a formula in the current state
      of the sat solver.
      @raise UndecidedLit if the literal is not decided *)

  val eval_level : St.formula -> bool * int
  (** Return the current assignement of the literals, as well as its
      decision level. If the level is 0, then it is necessary for
      the atom to have this value; otherwise it is due to choices
      that can potentially be backtracked.
      @raise UndecidedLit if the literal is not decided *)


  (** {2 Proofs and Models} *)

  module Proof : Res.S with module St = St

  val unsat_conflict : unit -> St.clause option
  (** Returns the unsat clause found at the toplevel, if it exists (i.e if
      [solve] has raised [Unsat]) *)

  val model : unit -> (St.term * St.term) list
  (** Returns the model found if the formula is satisfiable. *)


  (** {2 Internal data}
      These functions expose some internal data stored by the solver, as such
      great care should be taken to ensure not to mess with the values returned. *)

  val trail : unit -> St.t Vec.t
  (** Returns the current trail.
      *DO NOT MUTATE* *)

  val hyps : unit -> St.clause Vec.t
  (** Returns the vector of assumptions used by the solver. May be slightly different
      from the clauses assumed because of top-level simplification of clauses.
      *DO NOT MUTATE* *)

  val temp : unit -> St.clause Vec.t
  (** Returns the clauses coreesponding to the local assumptions.
      All clauses in this vec are assured to be unit clauses.
      *DO NOT MUTATE* *)

  val history : unit -> St.clause Vec.t
  (** Returns the history of learnt clauses, with no guarantees on order.
      *DO NOT MUTATE* *)

end

