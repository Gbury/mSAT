(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2016 Guillaume Bury
Copyright 2016 Simon Cruanes
*)

(** Interface for Solvers

    This modules defines the safe external interface for solvers.
    Solvers that implements this interface can be obtained using the [Make]
    functor in {!Solver} or {!Mcsolver}.
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
  iter_trail : ('form -> unit) -> ('term -> unit) -> unit;
  (** Iter thorugh the formulas and terms in order of decision/propagation
      (starting from the first propagation, to the last propagation). *)
  model: unit -> ('term * 'term) list;
  (** Returns the model found if the formula is satisfiable. *)
}
(** The type of values returned when the solver reaches a SAT state. *)

type ('clause, 'proof) unsat_state = {
  unsat_conflict : unit -> 'clause;
  (** Returns the unsat clause found at the toplevel *)
  get_proof : unit -> 'proof;
  (** returns a persistent proof of the empty clause from the Unsat result. *)
}
(** The type of values returned when the solver reaches an UNSAT state. *)

type 'clause export = {
  hyps: 'clause Vec.t;
  history: 'clause Vec.t;
  local: 'clause Vec.t;
}
(** Export internal state *)

(** The external interface implemented by safe solvers, such as the one
    created by the {!Solver.Make} and {!Mcsolver.Make} functors. *)
module type S = sig
  (** {2 Internal modules}
      These are the internal modules used, you should probably not use them
      if you're not familiar with the internals of mSAT. *)

  (* TODO: replace {!St} with explicit modules (Expr, Var, Lit, Elt,...)
     with carefully picked interfaces *)
  module St : Solver_types.S
  (** WARNING: Very dangerous module that expose the internal representation used
      by the solver. *)

  module Proof : Res.S with module St = St
  (** A module to manipulate proofs. *)

  (** {2 Types} *)

  type atom = St.formula
  (** The type of atoms given by the module argument for formulas *)

  type res =
    | Sat of (St.term,St.formula) sat_state         (** Returned when the solver reaches SAT *)
    | Unsat of (St.clause,Proof.proof) unsat_state  (** Returned when the solver reaches UNSAT *)
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

  val new_lit : St.term -> unit
  (** Add a new litteral (i.e term) to the solver. This term will
      be decided on at some point during solving, wether it appears
      in clauses or not. *)

  val new_atom : atom -> unit
  (** Add a new atom (i.e propositional formula) to the solver.
      This formula will be decided on at some point during solving,
      wether it appears in clauses or not. *)

  val unsat_core : Proof.proof -> St.clause list
  (** Returns the unsat core of a given proof. *)

  val true_at_level0 : atom -> bool
  (** [true_at_level0 a] returns [true] if [a] was proved at level0, i.e.
      it must hold in all models *)

  val get_tag : St.clause -> int option
  (** Recover tag from a clause, if any *)

  val export : unit -> St.clause export
end

