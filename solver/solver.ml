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

module type S = Solver_intf.S

module DummyTheory(F : Formula_intf.S) = struct
  (* We don't have anything to do since the SAT Solver already
   * does propagation and conflict detection *)

  type formula = F.t
  type proof = F.proof
  type level = unit

  type slice = {
    start : int;
    length : int;
    get : int -> formula;
    push : formula list -> proof -> unit;
  }

  type res =
    | Sat of level
    | Unsat of formula list * proof

  let dummy = ()
  let current_level () = ()
  let assume _ = Sat ()
  let backtrack _ = ()
end

module Plugin(E : Formula_intf.S)
    (Th : Theory_intf.S with type formula = E.t and type proof = E.proof) = struct

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


module Make (E : Formula_intf.S)
    (Th : Theory_intf.S with type formula = E.t and type proof = E.proof)
    (Dummy: sig end) = struct

  module P = Plugin(E)(Th)

  module St = Solver_types.SatMake(E)(struct end)

  module S = Internal.Make(St)(P)(struct end)

  module Proof = S.Proof

  exception UndecidedLit = S.UndecidedLit

  type atom = E.t
  type clause = St.clause
  type proof = Proof.proof

  type res = Sat | Unsat

  let assume ?tag l =
    try S.assume ?tag l
    with S.Unsat -> ()

  let solve () =
    try
      S.solve ();
      Sat
    with S.Unsat -> Unsat

  let eval = S.eval
  let eval_level = S.eval_level

  let get_proof () =
    match S.unsat_conflict () with
    | None -> assert false
    | Some c -> S.Proof.prove_unsat c

  let unsat_core = S.Proof.unsat_core

  let get_tag cl = St.(cl.tag)

  (* Push/pop operations *)
  type level = S.level
  let base_level = S.base_level
  let current_level = S.current_level
  let push = S.push
  let pop = S.pop
  let reset = S.reset

end

