(**************************************************************************)
(*                                                                        *)
(*                                  Cubicle                               *)
(*             Combining model checking algorithms and SMT solvers        *)
(*                                                                        *)
(*                  Sylvain Conchon and Alain Mebsout                     *)
(*                  Universite Paris-Sud 11                               *)
(*                                                                        *)
(*  Copyright 2011. This file is distributed under the terms of the       *)
(*  Apache Software License version 2.0                                   *)
(*                                                                        *)
(**************************************************************************)

open Num

type t

exception NotConsistent of Explanation.t
exception Not_a_float

val undefined : Ty.t -> t

val point : num -> Ty.t -> Explanation.t -> t

val doesnt_contain_0 : t -> Sig.answer

val is_strict_smaller : t -> t -> bool

val new_borne_sup : Explanation.t -> num -> is_le : bool -> t -> t

val new_borne_inf : Explanation.t -> num -> is_le : bool -> t -> t

val is_point : t -> (num * Explanation.t) option

val intersect : t -> t -> t

val exclude : t -> t -> t

val mult : t -> t -> t

val power : int -> t -> t

val sqrt : t -> t

val root : int -> t -> t 

val add : t -> t -> t

val scale : num -> t -> t

val print : Format.formatter -> t -> unit

val finite_size : t -> num option

val borne_inf : t -> num * Explanation.t

val div : t -> t -> t
