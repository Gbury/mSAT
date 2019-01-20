(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2016 Guillaume Bury
Copyright 2016 Simon Cruanes
*)

module type S = Solver_intf.S

module Make_cdcl_t = Internal.Make_cdcl_t
module Make_mcsat = Internal.Make_mcsat
module Make_pure_sat = Internal.Make_pure_sat

module Proof_empty
  : Solver_intf.PROOF with type _ t = unit
= struct
  type _ t = unit
  module Builder(A : Solver_intf.PROOF_ARG) = struct
    type ctx = unit
    type res_proof = unit
    type proof = unit
    let create() = ()
    let make_axiom _ = ()
    let make_simplify _ _ _ = ()
    let make_lemma _ _ = ()
    let res_init _ _ _ = ()
    let res_init_assumption _ _ = ()
    let res_step _ _ _ _ = ()
    let res_step_assumption _ _ _ = ()
    let res_finalize _ _ = ()
    let pp out _ = Format.fprintf out "<empty proof>"
  end [@@specialise] [@@inline]
end
