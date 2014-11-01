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
  type explanation

  exception Inconsistent of explanation

  val dummy : t
  val empty : unit -> t
  val assume : cs:bool -> formula -> explanation -> t -> t
  (** Any valid theory, either the empty one, or one returned by assume, should
      be different from the dummy one. *)
end

