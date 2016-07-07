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
module ThI = Theory_intf

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
        ThI.
        start = s.start;
        length = s.length;
        get = assume_get s;
        push = s.push;
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
    (Dummy: sig end) =
  External.Make
    (Solver_types.SatMake(E)(struct end))
    (Plugin(E)(Th))
    (struct end)


