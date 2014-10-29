(**************************************************************************)
(*                                                                        *)
(*                                  Cubicle                               *)
(*             Combining model checking algorithms and SMT solvers        *)
(*                                                                        *)
(*                  Sylvain Conchon, Evelyne Contejean                    *)
(*                  Francois Bobot, Mohamed Iguernelala, Alain Mebsout    *)
(*                  CNRS, Universite Paris-Sud 11                         *)
(*                                                                        *)
(*  Copyright 2011. This file is distributed under the terms of the       *)
(*  Apache Software License version 2.0                                   *)
(*                                                                        *)
(**************************************************************************)

module type S = sig
    type t
    type formula

    module St : Solver_types.S with type formula = formula
    module Ex : Explanation.S with type atom = St.atom

    exception Inconsistent of Ex.t

    val empty : unit -> t
    val assume : cs:bool -> formula -> Ex.t -> t -> t
end

