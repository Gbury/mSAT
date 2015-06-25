(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module Make (L : Log_intf.S)(E : Expr_intf.S)
    (Th : Plugin_intf.S with type term = E.Term.t and type formula = E.Formula.t) = struct

  module St = Solver_types.Make(L)(E)(Th)

  module M = Internal.Make(L)(St)(Th)

  include St

  include M

end

