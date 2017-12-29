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

module type S = Msat.S

module DummyTheory(F : Formula_intf.S) = struct
  (* We don't have anything to do since the SAT Solver already
   * does propagation and conflict detection *)

  type formula = F.t
  type proof = F.proof
  type level = unit

  let dummy = ()
  let current_level () = ()
  let assume _ = Theory_intf.Sat
  let backtrack _ = ()
  let if_sat _ = Theory_intf.Sat
end

module Plugin(E : Formula_intf.S)
    (Th : Theory_intf.S with type formula = E.t and type proof = E.proof) = struct

  type term = E.t
  type formula = E.t
  type proof = Th.proof
  type level = Th.level

  let dummy = Th.dummy

  let current_level = Th.current_level

  let assume_get get =
    function i ->
    match get i with
    | Plugin_intf.Lit f -> f
    | _ -> assert false

  let assume_propagate propagate =
    fun f l proof ->
      propagate f (Plugin_intf.Consequence (l, proof))

  let mk_slice s = {
    Theory_intf.start = s.Plugin_intf.start;
    length = s.Plugin_intf.length;
    get = assume_get s.Plugin_intf.get;
    push = s.Plugin_intf.push;
    propagate = assume_propagate s.Plugin_intf.propagate;
  }

  let assume s = Th.assume (mk_slice s)

  let backtrack = Th.backtrack

  let if_sat s = Th.if_sat (mk_slice s)


  (* McSat specific functions *)
  let assign _ = assert false

  let iter_assignable _ _ = ()

  let eval _ = Plugin_intf.Unknown

end


module Make (E : Formula_intf.S)
    (Th : Theory_intf.S with type formula = E.t and type proof = E.proof)
    = Msat.Make (Make_smt_expr(E)) (Plugin(E)(Th))


