(**************************************************************************)
(*                                                                        *)
(*                                  Cubicle                               *)
(*             Combining model checking algorithms and SMT solvers        *)
(*                                                                        *)
(*                  Mohamed Iguernelala                                   *)
(*                  Universite Paris-Sud 11                               *)
(*                                                                        *)
(*  Copyright 2011. This file is distributed under the terms of the       *)
(*  Apache Software License version 2.0                                   *)
(*                                                                        *)
(**************************************************************************)

type t

val init : int -> t
val in_heap : t -> int -> bool
val decrease : (int -> int -> bool) -> t -> int -> unit
(*val increase : (int -> int -> bool) -> t -> int -> unit*)
val size : t -> int
val is_empty : t -> bool
val insert : (int -> int -> bool) -> t -> int -> unit
val grow_to_by_double: t -> int -> unit
(*val update : (int -> int -> bool) -> t -> int -> unit*)
val remove_min : (int -> int -> bool) -> t -> int
val filter : t -> (int -> bool) -> (int -> int -> bool) -> unit
