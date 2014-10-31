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

module type S = sig
    type formula

    type var = {
        vid : int;
        pa : atom;
        na : atom;
        mutable weight : float;
        mutable seen : bool;
        mutable level : int;
        mutable reason : reason;
        mutable vpremise : premise
    }

    and atom = {
        var : var;
        lit : formula;
        neg : atom;
        mutable watched : clause Vec.t;
        mutable is_true : bool;
        aid : int
    }

    and clause = {
        name : string;
        mutable atoms : atom Vec.t;
        mutable activity : float;
        mutable removed : bool;
        learnt : bool;
        cpremise : premise
    }

    and reason = clause option

    and premise = clause list

    val cpt_mk_var : int ref
    type varmap
    val ma : varmap ref

    val dummy_var : var
    val dummy_atom : atom
    val dummy_clause : clause

    val make_var : formula -> var * bool

    val add_atom : formula -> atom

    val make_clause : string -> atom list -> int -> bool -> premise -> clause

    val fresh_name : unit -> string

    val fresh_lname : unit -> string

    val fresh_dname : unit -> string

    val to_float : int -> float

    val to_int : float -> int
    val made_vars_info : unit -> int * var list
    val clear : unit -> unit

    val pp_atom : Format.formatter -> atom -> unit
    val pp_clause : Format.formatter -> clause -> unit
end

