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

type 'a printer = Format.formatter -> 'a -> unit

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

type ('atom, 'clause, 'proof) unsat_state = {
  unsat_conflict : unit -> 'clause;
  (** Returns the unsat clause found at the toplevel *)
  get_proof : unit -> 'proof;
  (** returns a persistent proof of the empty clause from the Unsat result. *)
  unsat_assumptions: unit -> 'atom list;
  (** Subset of assumptions responsible for "unsat" *)
}
(** The type of values returned when the solver reaches an UNSAT state. *)

type 'clause export = {
  hyps: 'clause Vec.t;
  history: 'clause Vec.t;
}
(** Export internal state *)

type negated =
  | Negated     (** changed sign *)
  | Same_sign   (** kept sign *)
(** This type is used during the normalisation of formulas.
    See {!val:Expr_intf.S.norm} for more details. *)

type 'term eval_res =
  | Unknown                       (** The given formula does not have an evaluation *)
  | Valued of bool * ('term list) (** The given formula can be evaluated to the given bool.
                                      The list of terms to give is the list of terms that
                                      were effectively used for the evaluation. *)
(** The type of evaluation results for a given formula.
    For instance, let's suppose we want to evaluate the formula [x * y = 0], the
    following result are correct:
    - [Unknown] if neither [x] nor [y] are assigned to a value
    - [Valued (true, [x])] if [x] is assigned to [0]
    - [Valued (true, [y])] if [y] is assigned to [0]
    - [Valued (false, [x; y])] if [x] and [y] are assigned to 1 (or any non-zero number)
*)

type ('formula, 'proof) th_res =
  | Th_sat
  (** The current set of assumptions is satisfiable. *)
  | Th_unsat of 'formula list * 'proof
  (** The current set of assumptions is *NOT* satisfiable, and here is a
      theory tautology (with its proof), for which every litteral is false
      under the current assumptions. *)
(** Type returned by the theory. Formulas in the unsat clause must come from the
    current set of assumptions, i.e must have been encountered in a slice. *)

type ('term, 'formula) assumption =
  | Lit of 'formula         (** The given formula is asserted true by the solver *)
  | Assign of 'term * 'term (** The first term is assigned to the second *)
(** Asusmptions made by the core SAT solver. *)

type ('term, 'formula, 'proof) reason =
  | Eval of 'term list                      (** The formula can be evalutaed using the terms in the list *)
  | Consequence of 'formula list * 'proof   (** [Consequence (l, p)] means that the formulas in [l] imply
                                                the propagated formula [f]. The proof should be a proof of
                                                the clause "[l] implies [f]". *)
(** The type of reasons for propagations of a formula [f]. *)

type ('term, 'formula, 'proof) slice = {
  start : int;                                (** Start of the slice *)
  length : int;                               (** Length of the slice *)
  get : int -> ('term, 'formula) assumption;  (** Accessor for the assertions in the slice.
                                                  Should only be called on integers [i] s.t.
                                                  [start <= i < start + length] *)
  push : 'formula list -> 'proof -> unit;     (** Add a clause to the solver. *)
  propagate : 'formula ->
    ('term, 'formula, 'proof) reason -> unit; (** Propagate a formula, i.e. the theory can
                                                  evaluate the formula to be true (see the
                                                  definition of {!type:eval_res} *)
}
(** The type for a slice of assertions to assume/propagate in the theory. *)

type ('a, 'b) gadt_eq = GADT_EQ : ('a, 'a) gadt_eq

type void = (unit,bool) gadt_eq
(** A provably empty type *)

module type FORMULA = sig
  (** formulas *)

  type t
  (** The type of atomic formulas over terms. *)

  val equal : t -> t -> bool
  (** Equality over formulas. *)

  val hash : t -> int
  (** Hashing function for formulas. Should be such that two formulas equal according
      to {!val:Expr_intf.S.equal} have the same hash. *)

  val pp : t printer
  (** Printing function used among other thing for debugging.  *)

  val neg : t -> t
  (** Formula negation *)

  val norm : t -> t * negated
  (** Returns a 'normalized' form of the formula, possibly negated
      (in which case return [Negated]).
      [norm] must be so that [a] and [neg a] normalise to the same formula,
      but one returns [Negated] and the other [Same_sign]. *)
end

(** Formulas and Terms required for mcSAT *)
module type EXPR = sig
  module Term : sig
    type t
    (** The type of terms *)

    val equal : t -> t -> bool
    (** Equality over terms. *)

    val hash : t -> int
    (** Hashing function for terms. Should be such that two terms equal according
        to {!val:Expr_intf.S.equal} have the same hash. *)

    val pp : t printer
    (** Printing function used among other for debugging. *)
  end

  module Formula : FORMULA
end

module type PROOF_ARG = sig
  module Formula : sig
    type t
    val pp : t printer
  end
  module Atom : sig
    type t
    val formula : t -> Formula.t
    val pp : t printer
  end
  module Clause : sig
    type t
    val atoms_l : t -> Atom.t list
    val pp : t printer
    module Tbl : Hashtbl.S with type key = t
  end

  type theory_lemma
end

module type PROOF = sig
  type 'lemma t

  module Builder(A: PROOF_ARG) : sig
    type ctx
    type builder
    type proof = A.theory_lemma t

    val create : unit -> ctx
    (** Create a (possibly mutable) context that will be stored
        in the SAT solver *)

    val init_axiom : ctx -> builder
    (** Initialize a proof using a user input clause *)

    val init_assumption : ctx -> A.Formula.t -> builder
    (** Make a builder from an initial assumption *)

    val init_lemma : ctx -> A.theory_lemma -> builder

    val res_step : ctx -> pivot:A.Formula.t -> A.Clause.t -> builder -> builder
    (** Resolution step. Several such steps can be chained before obtaining a conclusion. *)

    val make : ctx -> conclusion:A.Clause.t -> builder -> A.theory_lemma t
    (** Give the resulting clause to the builder and obtain a proof *)

    val pp : proof printer
  end
end

(** Signature for theories to be given to the CDCL(T) solver *)
module type PLUGIN_CDCL_T = sig
  type t
  (** The plugin state itself *)

  module Formula : FORMULA

  type lemma
  (** Lemma for this theory *)

  module Proof : PROOF
  (** Builder for proofs *)

  type level
  (** The type for levels to allow backtracking. *)

  val current_level : t -> level
  (** Return the current level of the theory (either the empty/beginning state, or the
      last level returned by the [assume] function). *)

  val assume : t -> (void, Formula.t, lemma) slice -> (Formula.t, lemma) th_res
  (** Assume the formulas in the slice, possibly pushing new formulas to be propagated,
      and returns the result of the new assumptions. *)

  val if_sat : t -> (void, Formula.t, lemma) slice -> (Formula.t, lemma) th_res
  (** Called at the end of the search in case a model has been found. If no new clause is
      pushed and the function returns [Sat], then proof search ends and 'sat' is returned,
      else search is resumed. *)

  val backtrack : t -> level -> unit
  (** Backtrack to the given level. After a call to [backtrack l], the theory should be in the
      same state as when it returned the value [l], *)

end

(** Signature for theories to be given to the Model Constructing Solver. *)
module type PLUGIN_MCSAT = sig
  type t
  (** The plugin state itself *)

  include EXPR

  module Proof : PROOF
  (** How to build proofs *)

  type lemma
  (** Lemma for this theory *)

  type level
  (** The type for levels to allow backtracking. *)

  val current_level : t -> level
  (** Return the current level of the theory (either the empty/beginning state, or the
      last level returned by the [assume] function). *)

  val assume : t -> (Term.t, Formula.t, lemma) slice -> (Formula.t, lemma) th_res
  (** Assume the formulas in the slice, possibly pushing new formulas to be propagated,
      and returns the result of the new assumptions. *)

  val if_sat : t -> (Term.t, Formula.t, lemma) slice -> (Formula.t, lemma) th_res
  (** Called at the end of the search in case a model has been found. If no new clause is
      pushed and the function returns [Sat], then proof search ends and 'sat' is returned,
      else search is resumed. *)

  val backtrack : t -> level -> unit
  (** Backtrack to the given level. After a call to [backtrack l], the theory should be in the
      same state as when it returned the value [l], *)

  val assign : t -> Term.t -> Term.t
  (** Returns an assignment value for the given term. *)

  val iter_assignable : t -> (Term.t -> unit) -> Formula.t -> unit
  (** An iterator over the subTerm.ts of a Formula.t that should be assigned a value (usually the poure subTerm.ts) *)

  val eval : t -> Formula.t -> Term.t eval_res
  (** Returns the evaluation of the Formula.t in the current assignment *)
end

module type PLUGIN_SAT = sig
  module Formula : FORMULA
  module Proof : PROOF
end

(** The external interface implemented by safe solvers, such as the one
    created by the {!Solver.Make} and {!Mcsolver.Make} functors. *)
module type S = sig
  (** {2 Internal modules}
      These are the internal modules used, you should probably not use them
      if you're not familiar with the internals of mSAT. *)

  include EXPR

  type term = Term.t (** user terms *)

  type formula = Formula.t (** user formulas *)

  type atom
  (** The type of atoms given by the module argument for formulas.
      An atom is a user-defined atomic formula whose truth value is
      picked by Msat. *)

  type clause

  type theory

  type lemma
  (** Theory lemmas *)

  type solver

  type proof
  (** Type of proof attached to clauses *)

  module Atom : sig
    type t = atom

    val equal : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
    val neg : t -> t
    val sign : t -> bool
    val abs : t -> t
    val formula : t -> formula
    val pp : t printer
  end

  module Clause : sig
    type t = clause

    val atoms : t -> atom array
    val atoms_l : t -> atom list
    val equal : t -> t -> bool
    val name : t -> string
    val proof : t -> proof

    val pp : t printer

    module Tbl : Hashtbl.S with type key = t
  end

  type t = solver
  (** Main solver type, containing all state for solving. *)

  val create : ?size:[`Tiny|`Small|`Big] -> theory -> t
  (** Create new solver
      @param theory the theory
      @param size the initial size of internal data structures. The bigger,
        the faster, but also the more RAM it uses. *)

  (** {2 Types} *)

  (** Result type for the solver *)
  type res =
    | Sat of (term,atom) sat_state (** Returned when the solver reaches SAT, with a model *)
    | Unsat of (atom,clause,proof) unsat_state (** Returned when the solver reaches UNSAT, with a proof *)

  exception UndecidedLit
  (** Exception raised by the evaluating functions when a literal
      has not yet been assigned a value. *)

  (** {2 Base operations} *)

  val assume : t -> formula list list -> unit
  (** Add the list of clauses to the current set of assumptions.
      Modifies the sat solver state in place. *)

  val add_clause : t -> atom list -> unit
  (** Lower level addition of clauses *)

  val add_clause_a : t -> atom array -> unit
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

  val make_atom : t -> formula -> atom
  (** Add a new atom (i.e propositional formula) to the solver.
      This formula will be decided on at some point during solving,
      wether it appears in clauses or not. *)

  val true_at_level0 : t -> atom -> bool
  (** [true_at_level0 a] returns [true] if [a] was proved at level0, i.e.
      it must hold in all models *)

  val export : t -> clause export
end

