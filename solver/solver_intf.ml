(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2016 Guillaume Bury
Copyright 2016 Simon Cruanes
*)

module type S = sig

  (** {2 Internal modules}
      These are the internal modules used, you should probablynot use them
      if you're not familiar with the internals of mSAT. *)

  module St : Solver_types.S

  module Proof : Res.S with module St = St

  (** {2 Types} *)

  type atom = St.formula
  (** The type of atoms given by the module argument for formulas *)

  type res = Sat | Unsat
  (** Result type for the solver *)

  exception UndecidedLit
  (** Exception raised by the evaluating functions when a literal
      has not yet been assigned a value. *)

  (** {2 Base operations} *)

  val assume : ?tag:int -> atom list list -> unit
  (** Add the list of clauses to the current set of assumptions.
      Modifies the sat solver state in place. *)

  val solve : unit -> res
  (** Try and solves the current set of assumptions.
      @return () if the current set of clauses is satisfiable
      @raise Unsat if a toplevel conflict is found *)

  val eval : atom -> bool
  (** Returns the valuation of a formula in the current state
      of the sat solver.
      @raise UndecidedLit if the literal is not decided *)

  val eval_level : atom -> bool * int
  (** Return the current assignement of the literals, as well as its
      decision level. If the level is 0, then it is necessary for
      the atom to have this value; otherwise it is due to choices
      that can potentially be backtracked.
      @raise UndecidedLit if the literal is not decided *)

  val get_proof : unit -> Proof.proof
  (** If the last call to [solve] returned [Unsat], then returns a persistent
      proof of the empty clause. *)

  val unsat_core : Proof.proof -> St.clause list
  (** Returns the unsat core of a given proof. *)

  val get_tag : St.clause -> int option
  (** Recover tag from a clause, if any *)

  (** {2 Push/Pop operations} *)

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

  val reset : unit -> unit
  (** Rest the state of the solver, i.e return to level {!base_level} *)

end

