
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

type ('term, 'formula) assumption = ('term, 'formula) Solver_intf.assumption =
  | Lit of 'formula
  | Assign of 'term * 'term (** The first term is assigned to the second *)

type ('term, 'formula, 'proof) reason = ('term, 'formula, 'proof) Solver_intf.reason =
  | Eval of 'term list
  | Consequence of 'formula list * 'proof

type ('term, 'formula, 'proof) slice = ('term, 'formula, 'proof) Solver_intf.slice = {
  iter_assumptions: (('term,'formula) assumption -> unit) -> unit;
  push : ?keep:bool -> 'formula list -> 'proof -> unit;
  raise_conflict: 'b. 'formula list -> 'proof -> 'b;
  propagate : 'formula -> ('term, 'formula, 'proof) reason -> unit;
}

type negated = Solver_intf.negated = Negated | Same_sign

module Make_mcsat = Solver.Make_mcsat
module Make_cdcl_t = Solver.Make_cdcl_t
module Make_pure_sat = Solver.Make_pure_sat

module Backtrackable_ref = Backtrackable_ref

(**/**)
module Vec = Vec
module Log = Log
(**/**)
