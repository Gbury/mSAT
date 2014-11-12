(* Copyright 2014 Guillaume Bury *)

module type S = sig
  (** Sinature for a module handling proof by resolution from sat solving traces *)

  type atom
  type clause
  type lemma
  (** Abstract types for atoms, clauses and theoriy-specific lemmas *)

  val is_proven : clause -> bool
  (** Returns [true] if the clause has a derivation in the current proof graph, and [false] otherwise. *)

  exception Insuficient_hyps
  val learn : clause Vec.t -> unit
  (** Learn and build proofs for the clause in the vector. Clauses in the vector should be in the order they were learned. *)

  val assert_can_prove_unsat : clause -> unit
  (** [assert_can_prove_unsat c] tries and prove the empty clause from [c]. [c] may be a learnt clause not yet proved.
      @raise Insuficient_hyps if it is impossible. *)

  type proof_node = {
    conclusion : clause;
    step : step;
  }
  and proof = unit -> proof_node
  and step =
    | Hypothesis
    | Lemma of lemma
    | Resolution of proof * proof * atom

  val prove_unsat : clause -> proof
  (** Given a conflict clause [c], returns a proof of the empty clause. Same as [assert_can_prove_unsat] but returns
      the proof if it succeeds.
      @raise Insuficient_hyps if it does not succeed. *)

  val unsat_core : proof -> clause list
  (** Returns the unsat_core of the given proof, i.e the lists of conclusions of all leafs of the proof. *)

  val print_dot : Format.formatter -> proof -> unit
  (** Print the given proof in dot format on the given formatter. *)

end
