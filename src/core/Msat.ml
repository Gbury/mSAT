(** Main API *)


module Solver_intf = Solver_intf

module type S = Solver_intf.S
module type FORMULA = Solver_intf.FORMULA
module type EXPR = Solver_intf.EXPR
module type PLUGIN_CDCL_T = Solver_intf.PLUGIN_CDCL_T
module type PLUGIN_MCSAT = Solver_intf.PLUGIN_MCSAT
module type PROOF = Solver_intf.PROOF

(** Empty type *)
type void = (unit,bool) Solver_intf.gadt_eq

type lbool = Solver_intf.lbool = L_true | L_false | L_undefined

type ('term, 'form, 'value) sat_state = ('term, 'form, 'value) Solver_intf.sat_state = {
  eval : 'form -> bool;
  eval_level : 'form -> bool * int;
  iter_trail : ('form -> unit) -> ('term -> unit) -> unit;
  model : unit -> ('term * 'value) list;
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

type ('term, 'formula, 'value) assumption = ('term, 'formula, 'value) Solver_intf.assumption =
  | Lit of 'formula  (** The given formula is asserted true by the solver *)
  | Assign of 'term * 'value (** The term is assigned to the value *)

type ('term, 'formula, 'proof) reason = ('term, 'formula, 'proof) Solver_intf.reason =
  | Eval of 'term list
  | Consequence of (unit -> 'formula list * 'proof)

type ('term, 'formula, 'value, 'proof) acts = ('term, 'formula, 'value, 'proof) Solver_intf.acts = {
  acts_iter_assumptions: (('term,'formula,'value) assumption -> unit) -> unit;
  acts_eval_lit: 'formula -> lbool;
  acts_mk_lit: ?default_pol:bool -> 'formula -> unit;
  acts_mk_term: 'term -> unit;
  acts_add_clause : ?keep:bool -> 'formula list -> 'proof -> unit;
  acts_raise_conflict: 'b. 'formula list -> 'proof -> 'b;
  acts_propagate : 'formula -> ('term, 'formula, 'proof) reason -> unit;
  acts_add_decision_lit: 'formula -> bool -> unit;
}

type negated = Solver_intf.negated = Negated | Same_sign

(** Print {!negated} values *)
let pp_negated out = function
  | Negated -> Format.fprintf out "negated"
  | Same_sign -> Format.fprintf out "same-sign"

(** Print {!lbool} values *)
let pp_lbool out = function
  | L_true -> Format.fprintf out "true"
  | L_false -> Format.fprintf out "false"
  | L_undefined -> Format.fprintf out "undefined"

exception No_proof = Solver_intf.No_proof

module Make_mcsat = Solver.Make_mcsat
module Make_cdcl_t = Solver.Make_cdcl_t
module Make_pure_sat = Solver.Make_pure_sat

(**/**)
module Vec = Vec
module Log = Log
(**/**)
