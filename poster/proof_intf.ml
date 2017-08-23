type proof = clause
and proof_node = {
  conclusion : clause;
  step : step;
}
and step =
  | Hypothesis
  | Assumption
  | Lemma of lemma
  | Duplicate of proof * atom list
  | Resolution of proof * proof * atom

val expand : proof -> proof_node
