(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module type S = sig
  (** Signature for a module handling proof by resolution from sat solving traces *)

  module St : Solver_types.S
  (** Module defining atom and clauses *)

  (** {3 Type declarations} *)

  exception Insuficient_hyps
  (** Raised when a complete resolution derivation cannot be found using the current hypotheses. *)

  type atom = St.atom
  type clause = St.clause
  type lemma = St.proof
  (** Abstract types for atoms, clauses and theory-specific lemmas *)

  type proof
  and proof_node = {
    conclusion : clause;
    step : step;
  }
  and step =
    | Hypothesis
    | Lemma of lemma
    | Resolution of proof * proof * atom
    (** Lazy type for proof trees. Proofs can be extended to proof nodes using functions defined later. *)

  (** {3 Resolution helpers} *)
  val to_list : clause -> atom list
  (** Returns the sorted list of atoms of a clause. *)

  val merge : atom list -> atom list -> atom list
  (** Merge two sorted atom list using a suitable comparison function. *)

  val resolve : atom list -> atom list * atom list
  (** Performs a "resolution step" on a sorted list of atoms.
      [resolve (List.merge l1 l2)] where [l1] and [l2] are sorted atom lists should return the pair
      [\[a\], l'], where [l'] is the result of the resolution of [l1] and [l2] over [a]. *)

  (** {3 Proof building functions} *)

  val prove : clause -> proof
  (** Same as 'learn', but works on single clauses instead of vectors. *)

  val prove_unsat : clause -> proof
  (** Given a conflict clause [c], returns a proof of the empty clause.
      @raise Insuficient_hyps if it does not succeed. *)

  (** {3 Proof Manipulation} *)

  val expand : proof -> proof_node
  (** Return the proof step at the root of a given proof. *)

  val fold : ('a -> proof_node -> 'a) -> 'a -> proof -> 'a
  (** [fold f acc p], fold [f] over the proof [p] and all its node. It is guaranteed that
      [f] is executed exactly once on each proof ndoe in the tree, and that the execution of
      [f] on a proof node happens after the execution on the children of the nodes. *)

  val unsat_core : proof -> clause list
  (** Returns the unsat_core of the given proof, i.e the lists of conclusions of all leafs of the proof.
      More efficient than using the [fold] function since it has access to the internal representation of proofs *)

  (** {3 Misc} *)

  val check : proof -> unit
  (** Check the contents of a proof. Mainly for internal use *)

  val print_clause : Format.formatter -> clause -> unit
  (** A nice looking printer for clauses, which sort the atoms before printing. *)

end
