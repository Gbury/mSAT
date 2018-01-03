(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

(** Interface for proofs *)

type 'a printer = Format.formatter -> 'a -> unit

module type S = sig
  (** Signature for a module handling proof by resolution from sat solving traces *)

  (** {3 Type declarations} *)

  exception Insuficient_hyps
  (** Raised when a complete resolution derivation cannot be found using the current hypotheses. *)

  type formula
  type atom
  type lemma
  type clause
  (** Abstract types for atoms, clauses and theory-specific lemmas *)

  type proof
  (** Lazy type for proof trees. Proofs are persistent objects, and can be
      extended to proof nodes using functions defined later. *)

  and proof_node = {
    conclusion : clause;  (** The conclusion of the proof *)
    step : step;          (** The reasoning step used to prove the conclusion *)
  }
  (** A proof can be expanded into a proof node, which show the first step of the proof. *)

  (** The type of reasoning steps allowed in a proof. *)
  and step =
    | Hypothesis
    (** The conclusion is a user-provided hypothesis *)
    | Assumption
    (** The conclusion has been locally assumed by the user *)
    | Lemma of lemma
    (** The conclusion is a tautology provided by the theory, with associated proof *)
    | Duplicate of proof * atom list
    (** The conclusion is obtained by eliminating multiple occurences of the atom in
        the conclusion of the provided proof. *)
    | Resolution of proof * proof * atom
    (** The conclusion can be deduced by performing a resolution between the conclusions
        of the two given proofs. The atom on which to perform the resolution is also given. *)

  (** {3 Proof building functions} *)

  val prove : clause -> proof
  (** Given a clause, return a proof of that clause.
      @raise Insuficient_hyps if it does not succeed. *)

  val prove_unsat : clause -> proof
  (** Given a conflict clause [c], returns a proof of the empty clause.
      @raise Insuficient_hyps if it does not succeed. *)

  val prove_atom : atom -> proof option
  (** Given an atom [a], returns a proof of the clause [[a]] if [a] is true at level 0 *)

  (** {3 Proof Nodes} *)

  val parents : step -> proof list
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

  val expand : proof -> proof_node
  (** Return the proof step at the root of a given proof. *)

  val conclusion : proof -> clause
  (** What is proved at the root of the clause *)

  val fold : ('a -> proof_node -> 'a) -> 'a -> proof -> 'a
  (** [fold f acc p], fold [f] over the proof [p] and all its node. It is guaranteed that
      [f] is executed exactly once on each proof node in the tree, and that the execution of
      [f] on a proof node happens after the execution on the parents of the nodes. *)

  val unsat_core : proof -> clause list
  (** Returns the unsat_core of the given proof, i.e the lists of conclusions
      of all leafs of the proof.
      More efficient than using the [fold] function since it has
      access to the internal representation of proofs *)

  (** {3 Misc} *)

  val check : proof -> unit
  (** Check the contents of a proof. Mainly for internal use *)

  module Clause : sig
    type t = clause
    val name : t -> string
    val atoms : t -> atom array
    val atoms_l : t -> atom list
    val pp : t printer
    (** A nice looking printer for clauses, which sort the atoms before printing. *)

    module Tbl : Hashtbl.S with type key = t
  end

  module Atom : sig
    type t = atom
    val is_pos : t -> bool
    val neg : t -> t
    val abs : t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val lit : t -> formula
    val pp : t printer
  end

  module Tbl : Hashtbl.S with type key = proof
end

module type FULL = sig
  module St : Solver_types.S
  (** Module defining atom and clauses *)

  include S with type atom = St.atom
             and type lemma = St.proof
             and type clause = St.clause
             and type formula = St.formula
end
