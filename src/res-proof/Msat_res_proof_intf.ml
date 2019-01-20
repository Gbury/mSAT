
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
    | Hypothesis
    (** The conclusion is a user-provided hypothesis *)
    | Assumption
    (** The conclusion has been locally assumed by the user *)
    | Lemma of lemma
    (** The conclusion is a tautology provided by the theory, with associated proof *)
    | Duplicate of t * atom list
    (** The conclusion is obtained by eliminating multiple occurences of the atom in
        the conclusion of the provided proof. *)
    | Resolution of t * t * atom
    (** The conclusion can be deduced by performing a resolution between the conclusions
        of the two given proofs. The atom on which to perform the resolution is also given. *)

  (** {3 Proof building functions} *)

  val prove : clause -> t
  (** Given a clause, return a proof of that clause.
      @raise Insuficient_hyps if it does not succeed. *)

  val prove_unsat : clause -> t
  (** Given a conflict clause [c], returns a proof of the empty clause.
      @raise Insuficient_hyps if it does not succeed. *)

  val prove_atom : atom -> t option
  (** Given an atom [a], returns a proof of the clause [[a]] if [a] is true at level 0 *)

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

  val check : t -> unit
  (** Check the contents of a proof. Mainly for internal use *)

  module Tbl : Hashtbl.S with type key = t
end
