(**************************************************************************)
(*                                                                        *)
(*                          Alt-Ergo Zero                                 *)
(*                                                                        *)
(*                  Sylvain Conchon and Alain Mebsout                     *)
(*                      Universite Paris-Sud 11                           *)
(*                                                                        *)
(*  Copyright 2011. This file is distributed under the terms of the       *)
(*  Apache Software License version 2.0                                   *)
(*                                                                        *)
(**************************************************************************)

module Make (L : Log_intf.S)(E : Formula_intf.S)
    (Th : Theory_intf.S with type formula = E.t) = struct

  module Expr = struct
    module Term = E
    module Formula = E
    include E
  end

  module Plugin = struct
    type term = E.t
    type formula = E.t
    type proof = Th.proof

    type assumption =
      | Lit of formula
      | Assign of term * term

    type slice = {
      start : int;
      length : int;
      get : int -> assumption * int;
      push : formula list -> proof -> unit;
      propagate : formula -> int -> unit;
    }

    type level = Th.level

    type res =
      | Sat
      | Unsat of formula list * proof

    type eval_res =
      | Valued of bool * int
      | Unknown

    let dummy = Th.dummy

    let current_level = Th.current_level

    let assume_get s i = match s.get i with
      | Lit f, _ -> f | _ -> assert false

    let assume s =
      match Th.assume {
          Th.start = s.start;
          Th.length = s.length;
          Th.get = assume_get s;
          Th.push = s.push;
        } with
      | Th.Sat _ -> Sat
      | Th.Unsat (l, p) -> Unsat (l, p)

    let backtrack = Th.backtrack

    let assign _ = assert false

    let iter_assignable _ _ = ()

    let eval _ = Unknown

    let if_sat _ = ()

    let proof_debug _ = assert false
  end

  module St = Solver_types.SatMake(L)(E)(Th)

  module S = Internal.Make(L)(St)(Plugin)

  include S

end

