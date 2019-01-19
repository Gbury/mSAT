(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2016 Guillaume Bury
Copyright 2016 Simon Cruanes
*)

module type S = Solver_intf.S

module Make_cdcl_t = Internal.Make_cdcl_t
module Make_mcsat = Internal.Make_mcsat
module Make_pure_sat = Internal.Make_pure_sat

