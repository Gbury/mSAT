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
    type builder = unit
    let init_axiom = ()
    let init_assumption _ = ()
    let init_lemma _ = ()
    let res_step ~pivot:_ _ _ = ()
    let make ~conclusion:_ _ = ()
  end [@@specialise] [@@inline]
end
