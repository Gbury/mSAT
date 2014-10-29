(**************************************************************************)
(*                                                                        *)
(*                                  Cubicle                               *)
(*             Combining model checking algorithms and SMT solvers        *)
(*                                                                        *)
(*                  Sylvain Conchon and Alain Mebsout                     *)
(*                  Stephane Lescuyer                                     *)
(*                  INRIA, Universite Paris-Sud 11                        *)
(*                                                                        *)
(*  Copyright 2011. This file is distributed under the terms of the       *)
(*  Apache Software License version 2.0                                   *)
(*                                                                        *)
(**************************************************************************)

module type S = sig

    type t
    type exp
    type atom

    val empty : t
    val singleton : atom -> t

    val union : t -> t -> t
    val merge : t -> t -> t

    val iter_atoms : (atom -> unit)  -> t -> unit
    val fold_atoms : (atom -> 'a -> 'a )  -> t -> 'a -> 'a

    val fresh_exp : unit -> int
    val add_fresh : int -> t -> t
    val remove_fresh : int -> t -> t option

    val print : Format.formatter -> t -> unit
end
