(* Copyright 2014 Guillaume Bury *)

module type S = sig

  type atom
  type clause
  type lemma

  val is_proven : clause -> bool
  (** Returns [true] if the clause has a derivation in the current proof graph, and [false] otherwise. *)

  exception Cannot
  val assert_can_prove_unsat : clause -> unit
  (** [prove_unsat c] tries and prove the empty clause from [c].
      @raise Cannot if it is impossible. *)

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
  (** Given a conflict clause [c], returns a proof of the empty clause. *)

  val print_dot : Format.formatter -> proof -> unit
  (** Print the given proof in dot format on the given formatter. *)

end
