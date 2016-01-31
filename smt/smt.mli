(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module Make(Dummy: sig end) : sig
  (** Fonctor to make a SMT Solver module with built-in literals. *)

  include Solver.S with type St.formula = Expr.t

  val print_clause : Format.formatter -> St.clause -> unit
  (** Print the clause on the given formatter. *)

  val print_proof : Format.formatter -> Proof.proof -> unit
  (** Print the given proof in dot format. *)

end

