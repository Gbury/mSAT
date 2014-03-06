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



type var = 
    { vid : int;
      pa : atom;
      na : atom;
      mutable weight : float;
      mutable seen : bool;
      mutable level : int;
      mutable reason : reason;
      mutable vpremise : premise }
    
and atom = 
    { var : var;
      lit : Literal.LT.t;
      neg : atom;
      mutable watched : clause Vec.t;
      mutable is_true : bool;
      aid : int }

and clause = 
    { name : string;
      mutable atoms : atom Vec.t;
      mutable activity : float;
      mutable removed : bool;
      learnt : bool;
      cpremise : premise }

and reason = clause option

and premise = clause list

module Make (Dummy : sig end) : sig

val cpt_mk_var : int ref
val ma : var Literal.LT.Map.t ref

val dummy_var : var
val dummy_atom : atom
val dummy_clause : clause

val make_var : Literal.LT.t -> var * bool

val add_atom : Literal.LT.t -> atom 

val make_clause : string -> atom list -> int -> bool -> premise-> clause

val fresh_name : unit -> string

val fresh_lname : unit -> string

val fresh_dname : unit -> string

val to_float : int -> float

val to_int : float -> int
val made_vars_info : unit -> int * var list
val clear : unit -> unit

end

module Debug: sig
    
  val atom : Format.formatter -> atom -> unit
    
  val clause : Format.formatter -> clause -> unit

end
