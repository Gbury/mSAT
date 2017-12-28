
(** Main API *)

module Formula_intf = Formula_intf
module Plugin_intf = Plugin_intf
module Theory_intf = Theory_intf
module Expr_intf = Expr_intf
module Solver_types_intf = Solver_types_intf

module Res = Res

module type S = Solver_intf.S

type ('term, 'form) sat_state = ('term, 'form) Solver_intf.sat_state = {
  eval : 'form -> bool;
  eval_level : 'form -> bool * int;
  iter_trail : ('form -> unit) -> ('term -> unit) -> unit;
  model : unit -> ('term * 'term) list;
}

type ('clause, 'proof) unsat_state = ('clause, 'proof) Solver_intf.unsat_state = {
  unsat_conflict : unit -> 'clause;
  get_proof : unit -> 'proof;
}
type 'clause export = 'clause Solver_intf.export = {
  hyps : 'clause Vec.t;
  history : 'clause Vec.t;
  local : 'clause Vec.t;
}

module Make_smt_expr(E : Formula_intf.S) = Solver_types.SatMake(E)
module Make_mcsat_expr(E : Expr_intf.S) = Solver_types.McMake(E)

module Make = Solver.Make

(**/**)
module Vec = Vec
module Log = Log
(**/**)
