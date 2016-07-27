(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2016 Guillaume Bury
Copyright 2016 Simon Cruanes
*)

type ('term, 'form) sat_state = {
  eval: 'form -> bool;
  (** Returns the valuation of a formula in the current state
      of the sat solver.
      @raise UndecidedLit if the literal is not decided *)
  eval_level: 'form -> bool * int;
  (** Return the current assignement of the literals, as well as its
      decision level. If the level is 0, then it is necessary for
      the atom to have this value; otherwise it is due to choices
      that can potentially be backtracked.
      @raise UndecidedLit if the literal is not decided *)
  model: unit -> ('term * 'term) list;
  (** Returns the model found if the formula is satisfiable. *)
}

type ('clause, 'proof) unsat_state = {
  unsat_conflict : unit -> 'clause;
  (** Returns the unsat clause found at the toplevel *)
  get_proof : unit -> 'proof;
  (** returns a persistent proof of the empty clause from the Unsat result. *)
}

module type S = sig

  (** {2 Internal modules}
      These are the internal modules used, you should probablynot use them
      if you're not familiar with the internals of mSAT. *)

  module St : Solver_types.S

  module Proof : Res.S with module St = St

  (** {2 Types} *)

  type atom = St.formula
  (** The type of atoms given by the module argument for formulas *)

  type res =
    | Sat of (St.term,St.formula) sat_state
    | Unsat of (St.clause,Proof.proof) unsat_state
  (** Result type for the solver *)

  exception UndecidedLit
  (** Exception raised by the evaluating functions when a literal
      has not yet been assigned a value. *)

  (** {2 Base operations} *)

  val assume : ?tag:int -> atom list list -> unit
  (** Add the list of clauses to the current set of assumptions.
      Modifies the sat solver state in place. *)

  val solve : ?assumptions:atom list -> unit -> res
  (** Try and solves the current set of assumptions. *)

  val unsat_core : Proof.proof -> St.clause list
  (** Returns the unsat core of a given proof. *)

  val get_tag : St.clause -> int option
  (** Recover tag from a clause, if any *)

end

