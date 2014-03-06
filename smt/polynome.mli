(**************************************************************************)
(*                                                                        *)
(*                                  Cubicle                               *)
(*             Combining model checking algorithms and SMT solvers        *)
(*                                                                        *)
(*                  Sylvain Conchon, Alain Mebsout                        *)
(*                  Mohamed Iguernelala                                   *)
(*                  Universite Paris-Sud 11                               *)
(*                                                                        *)
(*  Copyright 2011. This file is distributed under the terms of the       *)
(*  Apache Software License version 2.0                                   *)
(*                                                                        *)
(**************************************************************************)

open Num

exception Not_a_num
exception Maybe_zero

module type S = sig
  type r 
  val compare : r -> r-> int
  val term_embed : Term.t -> r
  val mult : r -> r -> r
  val print : Format.formatter -> r -> unit
end

module type T = sig

  type r 
  type t
  
  val compare : t -> t -> int
  val hash : t -> int

  val create : (num * r) list -> num -> Ty.t-> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mult : t -> t -> t
  val mult_const : num -> t -> t
  val div : t -> t -> t * bool
  val modulo : t -> t -> t

  val is_empty : t -> bool
  val find : r -> t -> num
  val choose : t -> num * r
  val subst : r -> t -> t -> t
  val remove : r -> t -> t
  val to_list : t -> (num * r) list * num
    
  val print : Format.formatter -> t -> unit
  val type_info : t -> Ty.t
  val is_monomial : t -> (num * r * num) option

  (* PPMC des denominateurs des coefficients excepte la constante *)
  val ppmc_denominators : t -> num
  (* PGCD des numerateurs des coefficients excepte la constante *)
  val pgcd_numerators : t -> num
  (* retourne un polynome sans constante et sa constante 
     et la constante multiplicative:
     normal_form p = (p',c,d) <=> p = (p' + c) * d *)
  val normal_form : t -> t * num * num
  (* comme normal_form mais le signe est aussi normalise *)
  val normal_form_pos : t -> t * num * num
end

module Make (X : S) : T with type r = X.r
	
