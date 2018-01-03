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

type 'a printer = Format.formatter -> 'a -> unit

(** The external interface implemented by safe solvers, such as the one
    created by the {!Solver.Make} and {!Mcsolver.Make} functors. *)
module type S = sig
  (** {2 Internal modules}
      These are the internal modules used, you should probably not use them
      if you're not familiar with the internals of mSAT. *)

  type term (** user terms *)

  type formula (** user formulas *)

  type clause

  module Proof : Res.S with type clause = clause
  (** A module to manipulate proofs. *)

  type t
  (** Main solver type, containing all state for solving. *)

  val create : ?size:[`Tiny|`Small|`Big] -> unit -> t
  (** Create new solver
      @param size the initial size of internal data structures. The bigger,
        the faster, but also the more RAM it uses. *)

  (** {2 Types} *)

  type atom = formula
  (** The type of atoms given by the module argument for formulas.
      An atom is a user-defined atomic formula whose truth value is
      picked by Msat. *)

  (** Result type for the solver *)
  type res =
    | Sat of (term,formula) sat_state (** Returned when the solver reaches SAT, with a model *)
    | Unsat of (clause,Proof.proof) unsat_state (** Returned when the solver reaches UNSAT, with a proof *)

  exception UndecidedLit
  (** Exception raised by the evaluating functions when a literal
      has not yet been assigned a value. *)

  (** {2 Base operations} *)

  val assume : t -> ?tag:int -> atom list list -> unit
  (** Add the list of clauses to the current set of assumptions.
      Modifies the sat solver state in place. *)

  val add_clause : t -> clause -> unit
  (** Lower level addition of clauses *)

  val solve : t -> ?assumptions:atom list -> unit -> res
  (** Try and solves the current set of clauses.
      @param assumptions additional atomic assumptions to be temporarily added.
        The assumptions are just used for this call to [solve], they are
        not saved in the solver's state. *)

  val new_lit : t -> term -> unit
  (** Add a new litteral (i.e term) to the solver. This term will
      be decided on at some point during solving, wether it appears
      in clauses or not. *)

  val new_atom : t -> atom -> unit
  (** Add a new atom (i.e propositional formula) to the solver.
      This formula will be decided on at some point during solving,
      wether it appears in clauses or not. *)

  val unsat_core : Proof.proof -> clause list
  (** Returns the unsat core of a given proof, ie a subset of all the added
      clauses that is sufficient to establish unsatisfiability. *)

  val true_at_level0 : t -> atom -> bool
  (** [true_at_level0 a] returns [true] if [a] was proved at level0, i.e.
      it must hold in all models *)

  val get_tag : clause -> int option
  (** Recover tag from a clause, if any *)

  val push : t -> unit
  (** Push a new save point. Clauses added after this call to [push] will
      be added as normal, but the corresponding call to [pop] will
      remove these clauses. *)

  val pop : t -> unit
  (** Return to last save point, discarding clauses added since last
      call to [push] *)

  val export : t -> clause export

  (** {2 Re-export some functions} *)

  type solver = t

  module Clause : sig
    type t = clause

    val atoms : t -> atom array
    val tag : t -> int option
    val equal : t -> t -> bool

    val make : solver -> ?tag:int -> atom list -> t

    val pp : t printer
  end

  module Formula : sig
    type t = formula
    val pp : t printer
  end

  module Term : sig
    type t = term
    val pp : t printer
  end
end

