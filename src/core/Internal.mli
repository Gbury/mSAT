(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

(** mSAT core

    This is the core of msat, containing the code doing the actual solving.
    This module is based on mini-sat, and as such the solver heavily uses mutation,
    which makes using it direclty kinda tricky (some exceptions can be raised
    at surprising times, mutating is dangerous for maintaining invariants, etc...).
*)

module Make
  (St : Solver_types.S)
  (Th : Plugin_intf.S with type term = St.term
                       and type formula = St.formula and type proof = St.proof)
: sig
  (** Functor to create a solver parametrised by the atomic formulas and a theory. *)

  (** {2 Solving facilities} *)

  exception Unsat
  exception UndecidedLit

  type t
  (** Solver *)

  val create : ?size:[`Tiny|`Small|`Big] -> ?st:St.t -> unit -> t

  val st : t -> St.t
  (** Underlying state *)

  val solve : t -> unit
  (** Try and solves the current set of assumptions.
      @return () if the current set of clauses is satisfiable
      @raise Unsat if a toplevel conflict is found *)

  val assume : t -> ?tag:int -> St.formula list list -> unit
  (** Add the list of clauses to the current set of assumptions.
      Modifies the sat solver state in place. *)

  val new_lit : t -> St.term -> unit
  (** Add a new litteral (i.e term) to the solver. This term will
      be decided on at some point during solving, wether it appears
      in clauses or not. *)

  val new_atom : t -> St.formula -> unit
  (** Add a new atom (i.e propositional formula) to the solver.
      This formula will be decided on at some point during solving,
      wether it appears in clauses or not. *)

  val push : t -> unit
  (** Create a decision level for local assumptions.
      @raise Unsat if a conflict is detected in the current state. *)

  val pop : t -> unit
  (** Pop a decision level for local assumptions. *)

  val local : t -> St.formula list -> unit
  (** Add local assumptions
      @param assumptions list of additional local assumptions to make,
        removed after the callback returns a value *)

  (** {2 Propositional models} *)

  val eval : t -> St.formula -> bool
  (** Returns the valuation of a formula in the current state
      of the sat solver.
      @raise UndecidedLit if the literal is not decided *)

  val eval_level : t -> St.formula -> bool * int
  (** Return the current assignement of the literals, as well as its
      decision level. If the level is 0, then it is necessary for
      the atom to have this value; otherwise it is due to choices
      that can potentially be backtracked.
      @raise UndecidedLit if the literal is not decided *)

  val model : t -> (St.term * St.term) list
  (** Returns the model found if the formula is satisfiable. *)

  val check : t -> bool
  (** Check the satisfiability of the current model. Only has meaning
      if the solver finished proof search and has returned [Sat]. *)

  (** {2 Proofs and Models} *)

  module Proof : Res.S with module St = St

  val unsat_conflict : t -> St.clause option
  (** Returns the unsat clause found at the toplevel, if it exists (i.e if
      [solve] has raised [Unsat]) *)

  val full_slice : t -> (St.term, St.formula, St.proof) Plugin_intf.slice
  (** View the current state of the trail as a slice. Mainly useful when the
      solver has reached a SAT conclusion. *)

  (** {2 Internal data}
      These functions expose some internal data stored by the solver, as such
      great care should be taken to ensure not to mess with the values returned. *)

  val trail : t -> St.trail_elt Vec.t
  (** Returns the current trail.
      *DO NOT MUTATE* *)

  val hyps : t -> St.clause Vec.t
  (** Returns the vector of assumptions used by the solver. May be slightly different
      from the clauses assumed because of top-level simplification of clauses.
      *DO NOT MUTATE* *)

  val temp : t -> St.clause Vec.t
  (** Returns the clauses coreesponding to the local assumptions.
      All clauses in this vec are assured to be unit clauses.
      *DO NOT MUTATE* *)

  val history : t -> St.clause Vec.t
  (** Returns the history of learnt clauses, with no guarantees on order.
      *DO NOT MUTATE* *)

end

