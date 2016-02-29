(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module Make (E : Expr_intf.S)
    (Th : Plugin_intf.S with type term = E.Term.t and type formula = E.Formula.t and type proof = E.proof)
    (Dummy: sig end) = struct

  module St = Solver_types.McMake(E)

  module M = Internal.Make(St)(Th)()

  include St

  include M

end

