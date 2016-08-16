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
open Msat

type symbol = Symbols.t

type t =
  | Tbool
  | Tabstract of ID.t
  | Tsum of ID.t * symbol list

val bool : t
val abstract : ID.t -> t
val sum : ID.t -> symbol list -> t

val hash : t -> int
val equal : t -> t -> bool
val compare : t -> t -> int
val print : Format.formatter -> t -> unit
