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

type ('term, 'form, 'value) sat_state = {
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
  model: unit -> ('term * 'value) list;
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

type ('term, 'formula, 'value) assumption =
  | Lit of 'formula  (** The given formula is asserted true by the solver *)
  | Assign of 'term * 'value (** The term is assigned to the value *)
(** Asusmptions made by the core SAT solver. *)

type ('term, 'formula, 'proof) reason =
  | Eval of 'term list
  (** The formula can be evalutaed using the terms in the list *)
  | Consequence of (unit -> 'formula list * 'proof)
  (** [Consequence (l, p)] means that the formulas in [l] imply the propagated
      formula [f]. The proof should be a proof of the clause "[l] implies [f]".

      invariant: in [Consequence (fun () -> l,p)], all elements of [l] must be true in
      the current trail.

      {b note} on lazyiness: the justification is suspended (using [unit -> …])
      to avoid potentially costly computations that might never be used
      if this literal is backtracked without participating in a conflict.
      Therefore the function that produces [(l,p)] needs only be safe in
      trails (partial models) that are conservative extensions of the current
      trail.
      If the theory isn't robust w.r.t. extensions of the trail (e.g. if
      its internal state undergoes significant changes),
      it can be easier to produce the explanation eagerly when
      propagating, and then use [Consequence (fun () -> expl, proof)] with
      the already produced [(expl,proof)] tuple.
  *)
(** The type of reasons for propagations of a formula [f]. *)

type lbool = L_true | L_false | L_undefined
(** Valuation of an atom *)

(* TODO: find a way to use atoms instead of formulas here *)
type ('term, 'formula, 'value, 'proof) acts = {
  acts_iter_assumptions: (('term,'formula,'value) assumption -> unit) -> unit;
  (** Traverse the new assumptions on the boolean trail. *)

  acts_eval_lit: 'formula -> lbool;
  (** Obtain current value of the given literal *)

  acts_mk_lit: ?default_pol:bool -> 'formula -> unit;
  (** Map the given formula to a literal, which will be decided by the
      SAT solver. *)

  acts_mk_term: 'term -> unit;
  (** Map the given term (and its subterms) to decision variables,
      for the MCSAT solver to decide. *)

  acts_add_clause: ?keep:bool -> 'formula list -> 'proof -> unit;
  (** Add a clause to the solver.
      @param keep if true, the clause will be kept by the solver.
        Otherwise the solver is allowed to GC the clause and propose this
        partial model again.
  *)

  acts_raise_conflict: 'b. 'formula list -> 'proof -> 'b;
  (** Raise a conflict, yielding control back to the solver.
      The list of atoms must be a valid theory lemma that is false in the
      current trail. *)

  acts_propagate: 'formula -> ('term, 'formula, 'proof) reason -> unit;
  (** Propagate a formula, i.e. the theory can evaluate the formula to be true
      (see the definition of {!type:eval_res} *)

  acts_add_decision_lit: 'formula -> bool -> unit;
  (** Ask the SAT solver to decide on the given formula with given sign
      before it can answer [SAT]. The order of decisions is still unspecified.
      Useful for theory combination. This will be undone on backtracking. *)
}
(** The type for a slice of assertions to assume/propagate in the theory. *)

type ('a, 'b) gadt_eq = GADT_EQ : ('a, 'a) gadt_eq

type void = (unit,bool) gadt_eq
(** A provably empty type *)

exception No_proof

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
  type proof
  (** An abstract type for proofs *)

  module Term : sig
    type t
    (** The type of terms *)

    val equal : t -> t -> bool
    (** Equality over terms. *)

    val hash : t -> int
    (** Hashing function for terms. Should be such that two terms equal according
        to {!equal} have the same hash. *)

    val pp : t printer
    (** Printing function used among other for debugging. *)
  end

  module Value : sig
    type t
    (** The type of semantic values (domain elements) *)

    val pp : t printer
    (** Printing function used among other for debugging. *)
  end

  module Formula : FORMULA
end

(** Signature for theories to be given to the CDCL(T) solver *)
module type PLUGIN_CDCL_T = sig
  type t
  (** The plugin state itself *)

  module Formula : FORMULA

  type proof

  val push_level : t -> unit
  (** Create a new backtrack level *)

  val pop_levels : t -> int -> unit
  (** Pop [n] levels of the theory *)

  val partial_check : t -> (void, Formula.t, void, proof) acts -> unit
  (** Assume the formulas in the slice, possibly using the [slice]
      to push new formulas to be propagated or to raising a conflict or to add
      new lemmas. *)

  val final_check : t -> (void, Formula.t, void, proof) acts -> unit
  (** Called at the end of the search in case a model has been found.
      If no new clause is pushed, then proof search ends and "sat" is returned;
      if lemmas are added, search is resumed;
      if a conflict clause is added, search backtracks and then resumes. *)
end

(** Signature for theories to be given to the Model Constructing Solver. *)
module type PLUGIN_MCSAT = sig
  type t
  (** The plugin state itself *)

  include EXPR

  val push_level : t -> unit
  (** Create a new backtrack level *)

  val pop_levels : t -> int -> unit
  (** Pop [n] levels of the theory *)

  val partial_check : t -> (Term.t, Formula.t, Value.t, proof) acts -> unit
  (** Assume the formulas in the slice, possibly using the [slice]
      to push new formulas to be propagated or to raising a conflict or to add
      new lemmas. *)

  val final_check : t -> (Term.t, Formula.t, Value.t, proof) acts -> unit
  (** Called at the end of the search in case a model has been found.
      If no new clause is pushed, then proof search ends and "sat" is returned;
      if lemmas are added, search is resumed;
      if a conflict clause is added, search backtracks and then resumes. *)

  val assign : t -> Term.t -> Value.t
  (** Returns an assignment value for the given term. *)

  val iter_assignable : t -> (Term.t -> unit) -> Formula.t -> unit
  (** An iterator over the subTerm.ts of a Formula.t that should be assigned a value (usually the poure subTerm.ts) *)

  val eval : t -> Formula.t -> Term.t eval_res
  (** Returns the evaluation of the Formula.t in the current assignment *)
end

(** Signature for pure SAT solvers *)
module type PLUGIN_SAT = sig
  module Formula : FORMULA

  type proof
end

module type PROOF = sig
  (** Signature for a module handling proof by resolution from sat solving traces *)

  (** {3 Type declarations} *)

  exception Resolution_error of string
  (** Raised when resolution failed. *)

  type formula
  type atom
  type lemma
  type clause
  (** Abstract types for atoms, clauses and theory-specific lemmas *)

  type t
  (** Lazy type for proof trees. Proofs are persistent objects, and can be
      extended to proof nodes using functions defined later. *)

  and proof_node = {
    conclusion : clause;  (** The conclusion of the proof *)
    step : step;          (** The reasoning step used to prove the conclusion *)
  }
  (** A proof can be expanded into a proof node, which show the first step of the proof. *)

  (** The type of reasoning steps allowed in a proof. *)
  and step =
    | Hypothesis of lemma
    (** The conclusion is a user-provided hypothesis *)
    | Assumption
    (** The conclusion has been locally assumed by the user *)
    | Lemma of lemma
    (** The conclusion is a tautology provided by the theory, with associated proof *)
    | Duplicate of t * atom list
    (** The conclusion is obtained by eliminating multiple occurences of the atom in
        the conclusion of the provided proof. *)
    | Hyper_res of hyper_res_step

  and hyper_res_step = {
    hr_init: t;
    hr_steps: (atom * t) list; (* list of pivot+clause to resolve against [init] *)
  }

  (** {3 Proof building functions} *)

  val prove : clause -> t
  (** Given a clause, return a proof of that clause.
      @raise Resolution_error if it does not succeed. *)

  val prove_unsat : clause -> t
  (** Given a conflict clause [c], returns a proof of the empty clause.
      @raise Resolution_error if it does not succeed. *)

  val prove_atom : atom -> t option
  (** Given an atom [a], returns a proof of the clause [[a]] if [a] is true at level 0 *)

  val res_of_hyper_res : hyper_res_step -> t * t * atom
  (** Turn an hyper resolution step into a resolution step.
      The conclusion can be deduced by performing a resolution between the conclusions
      of the two given proofs.
      The atom on which to perform the resolution is also given. *)

  (** {3 Proof Nodes} *)

  val parents : step -> t list
  (** Returns the parents of a proof node. *)

  val is_leaf : step -> bool
  (** Returns wether the the proof node is a leaf, i.e. an hypothesis,
      an assumption, or a lemma.
      [true] if and only if {!parents} returns the empty list. *)

  val expl : step -> string
  (** Returns a short string description for the proof step; for instance
      ["hypothesis"] for a [Hypothesis]
      (it currently returns the variant name in lowercase). *)


  (** {3 Proof Manipulation} *)

  val expand : t -> proof_node
  (** Return the proof step at the root of a given proof. *)

  val conclusion : t -> clause
  (** What is proved at the root of the clause *)

  val fold : ('a -> proof_node -> 'a) -> 'a -> t -> 'a
  (** [fold f acc p], fold [f] over the proof [p] and all its node. It is guaranteed that
      [f] is executed exactly once on each proof node in the tree, and that the execution of
      [f] on a proof node happens after the execution on the parents of the nodes. *)

  val unsat_core : t -> clause list
  (** Returns the unsat_core of the given proof, i.e the lists of conclusions
      of all leafs of the proof.
      More efficient than using the [fold] function since it has
      access to the internal representation of proofs *)

  (** {3 Misc} *)

  val check_empty_conclusion : t -> unit
  (** Check that the proof's conclusion is the empty clause,
      @raise Resolution_error otherwise *)

  val check : t -> unit
  (** Check the contents of a proof. Mainly for internal use. *)

  module Tbl : Hashtbl.S with type key = t
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
  (** A theory lemma or an input axiom *)

  type solver

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

    val pp : t printer

    module Tbl : Hashtbl.S with type key = t
  end

  module Proof : PROOF
    with type clause = clause
     and type atom = atom
     and type formula = formula
     and type lemma = lemma
     and type t = proof
  (** A module to manipulate proofs. *)

  type t = solver
  (** Main solver type, containing all state for solving. *)

  val create :
    ?on_conflict:(atom array -> unit) ->
    ?on_decision:(atom -> unit) ->
    ?on_new_atom:(atom -> unit) ->
    ?store_proof:bool ->
    ?size:[`Tiny|`Small|`Big] ->
    theory ->
    t
  (** Create new solver
      @param theory the theory
      @param store_proof if true, stores proof (default [true]). Otherwise
        the functions that return proofs will fail with [No_proof]
      @param size the initial size of internal data structures. The bigger,
        the faster, but also the more RAM it uses. *)

  val theory : t -> theory
  (** Access the theory state *)

  (** {2 Types} *)

  (** Result type for the solver *)
  type res =
    | Sat of (term,formula,Value.t) sat_state (** Returned when the solver reaches SAT, with a model *)
    | Unsat of (atom,clause,Proof.t) unsat_state (** Returned when the solver reaches UNSAT, with a proof *)

  exception UndecidedLit
  (** Exception raised by the evaluating functions when a literal
      has not yet been assigned a value. *)

  (** {2 Base operations} *)

  val assume : t -> formula list list -> lemma -> unit
  (** Add the list of clauses to the current set of assumptions.
      Modifies the sat solver state in place. *)

  val add_clause : t -> atom list -> lemma -> unit
  (** Lower level addition of clauses *)

  val add_clause_a : t -> atom array -> lemma -> unit
  (** Lower level addition of clauses *)

  val solve :
    ?assumptions:atom list ->
    t -> res
  (** Try and solves the current set of clauses.
      @param assumptions additional atomic assumptions to be temporarily added.
        The assumptions are just used for this call to [solve], they are
        not saved in the solver's state. *)

  val make_term : t -> term -> unit
  (** Add a new term (i.e. decision variable) to the solver. This term will
      be decided on at some point during solving, wether it appears
      in clauses or not. *)

  val make_atom : t -> formula -> atom
  (** Add a new atom (i.e propositional formula) to the solver.
      This formula will be decided on at some point during solving,
      wether it appears in clauses or not. *)

  val true_at_level0 : t -> atom -> bool
  (** [true_at_level0 a] returns [true] if [a] was proved at level0, i.e.
      it must hold in all models *)

  val eval_atom : t -> atom -> lbool
  (** Evaluate atom in current state *)

  val export : t -> clause export
end

