
(** Main API *)


module Solver_intf = Solver_intf

module type S = Solver_intf.S
module type FORMULA = Solver_intf.FORMULA
module type EXPR = Solver_intf.EXPR
module type PLUGIN_CDCL_T = Solver_intf.PLUGIN_CDCL_T
module type PLUGIN_MCSAT = Solver_intf.PLUGIN_MCSAT
module type PROOF = Solver_intf.PROOF

type ('term, 'form) sat_state = ('term, 'form) Solver_intf.sat_state = {
  eval : 'form -> bool;
  eval_level : 'form -> bool * int;
  iter_trail : ('form -> unit) -> ('term -> unit) -> unit;
  model : unit -> ('term * 'term) list;
}

type ('atom,'clause, 'proof) unsat_state = ('atom,'clause, 'proof) Solver_intf.unsat_state = {
  unsat_conflict : unit -> 'clause;
  get_proof : unit -> 'proof;
  unsat_assumptions: unit -> 'atom list;
}
type 'clause export = 'clause Solver_intf.export = {
  hyps : 'clause Vec.t;
  history : 'clause Vec.t;
}
type ('formula, 'proof) th_res = ('formula, 'proof) Solver_intf.th_res =
  | Th_sat
  | Th_unsat of 'formula list * 'proof

type negated = Solver_intf.negated = Negated | Same_sign

module Make_mcsat = Solver.Make_mcsat
module Make_cdcl_t = Solver.Make_cdcl_t
module Make_pure_sat = Solver.Make_pure_sat

(**/**)
module Vec = Vec
module Log = Log
(**/**)
