(**************************************************************************)
(*                                                                        *)
(*                          Alt-Ergo Zero                                 *)
(*                                                                        *)
(*                  Sylvain Conchon and Alain Mebsout                     *)
(*                      Universite Paris-Sud 11                           *)
(*                                                                        *)
(*  Copyright 2011. This file is distributed under the terms of the       *)
(*  Apache Software License version 2.0                                   *)
(*                                                                        *)
(**************************************************************************)

(** {b The Alt-Ergo Zero SMT library}

    This SMT solver is derived from {{:http://alt-ergo.lri.fr} Alt-Ergo}. It
    uses an efficient SAT solver and supports the following quantifier free
    theories:
    - Equality and uninterpreted functions
    - Arithmetic (linear, non-linear, integer, reals)
    - Enumerated data-types

    This API makes heavy use of hash-consed strings. Please take a moment to
    look at {! Hstring}.
*)

(** {2 Error handling } *)

type error =
  | DuplicateTypeName of Hstring.t (** raised when a type is already declared *)
  | DuplicateSymb of Hstring.t (** raised when a symbol is already declared *)
  | UnknownType of Hstring.t (** raised when the given type is not declared *)
  | UnknownSymb of Hstring.t (** raised when the given symbol is not declared *)

exception Error of error

(** {2 Typing } *)

(** {3 Typing } *)
module Type : sig

  type t = Hstring.t
  (** The type of types in Alt-Ergo Zero *)

  (** {4 Builtin types } *)

  val type_int : t
  (** The type of integers *)

  val type_real : t
  (** The type of reals *)

  val type_bool : t
  (** The type of booleans *)

  val type_proc : t
  (** The type processes (identifiers) *)

  (** {4 Declaring new types } *)

  val declare : Hstring.t -> Hstring.t list -> unit
  (** {ul {- [declare n cstrs] declares a new enumerated data-type with
      name [n] and constructors [cstrs].}
      {- [declare n []] declares a new abstract type with name [n].}}*)

  val all_constructors : unit -> Hstring.t list
  (** [all_constructors ()] returns a list of all the defined constructors. *)

  val  constructors : t -> Hstring.t list
  (** [constructors ty] returns the list of constructors of [ty] when type is
      an enumerated data-type, otherwise returns [[]].*)

end


(** {3 Function symbols} *)
module Symbol : sig
    
  type t = Hstring.t
  (** The type of function symbols *)
    
  val declare : Hstring.t -> Type.t list -> Type.t -> unit
  (** [declare s [arg_1; ... ; arg_n] out] declares a new function
      symbol with type [ (arg_1, ... , arg_n) -> out] *)
    
  val type_of : t -> Type.t list * Type.t
    (** [type_of x] returns the type of x. *)
    
  val has_abstract_type : t -> bool
    (** [has_abstract_type x] is [true] if the type of x is abstract. *)
    
  val has_type_proc : t -> bool
  (** [has_type_proc x] is [true] if x has the type of a process
      identifier. *)
        
  val declared : t -> bool
  (** [declared x] is [true] if [x] is already declared. *)
      
end

(** {3 Variants}
   
    The types of symbols (when they are enumerated data types) can be refined
    to substypes of their original type (i.e. a subset of their constructors).
*)
module Variant : sig

  val init : (Symbol.t * Type.t) list -> unit
  (** [init l] where [l] is a list of pairs [(s, ty)] initializes the
      type (and associated constructors) of each [s] to its original type [ty].
      
      This function must be called with a list of all symbols before
      attempting to refine the types. *)

  val close : unit -> unit
  (** [close ()] will compute the smallest type possible for each symbol.

      This function must be called when all information has been added.*)

  val assign_constr : Symbol.t -> Hstring.t -> unit
    (** [assign_constr s cstr] will add the constraint that the constructor 
        [cstr] must be in the type of [s] *)

  val assign_var : Symbol.t -> Symbol.t -> unit
    (** [assign_var x y] will add the constraint that the type of [y] is a
        subtype of [x] (use this function when [x := y] appear in your 
        program *)

  val print : unit -> unit
    (** [print ()] will output the computed refined types on std_err. This
        function is here only for debugging purposes. Use it afer [close ()].*)

  val get_variants : Symbol.t -> Hstring.HSet.t
  (** [get_variants s] returns a set of constructors, which is the refined
      type of [s].*)

end

(** {2 Building terms} *)

module rec Term : sig

  type t
  (** The type of terms *)

  (** The type of operators *)
  type operator = 
    | Plus (** [+] *)
    | Minus (** [-] *)
    | Mult (** [*] *) 
    | Div (** [/] *)
    | Modulo (** [mod] *)

  val make_int : Num.num -> t
  (** [make_int n] creates an integer constant of value [n]. *)

  val make_real : Num.num -> t
  (** [make_real n] creates an real constant of value [n]. *)

  val make_app : Symbol.t -> t list -> t
  (** [make_app f l] creates the application of function symbol [f] to a list
      of terms [l]. *)

  val make_arith : operator -> t -> t -> t
  (** [make_arith op t1 t2] creates the term [t1 <op> t2]. *)

  val make_ite : Formula.t -> t -> t -> t
  (** [make_ite f t1 t2] creates the term [if f then t1 else t2]. *)

  val is_int : t -> bool
  (** [is_int x] is [true] if the term [x] has type int *)

  val is_real : t -> bool
  (** [is_real x] is [true] if the term [x] has type real *)

  val t_true : t
  (** [t_true] is the boolean term [true] *)

  val t_false : t
  (** [t_false] is the boolean term [false] *)

end


(** {2 Building formulas} *)

and Formula : sig

  (** The type of comparators: *)
  type comparator = 
    | Eq  (** equality ([=]) *)
    | Neq (** disequality ([<>]) *)
    | Le  (** inequality ([<=]) *)
    | Lt  (** strict inequality ([<]) *)

  (** The type of operators *)
  type combinator = 
    | And (** conjunction *)
    | Or  (** disjunction *)
    | Imp (** implication *)
    | Not (** negation *)

  (** The type of ground formulas *)
  type t = 
    | Lit of Literal.LT.t  
    | Comb of combinator * t list

  val f_true : t
  (** The formula which represents [true]*)

  val f_false : t
  (** The formula which represents [false]*)

  val make_lit : comparator -> Term.t list -> t
  (** [make_lit cmp [t1; t2]] creates the literal [(t1 <cmp> t2)]. *)

  val make : combinator -> t list -> t
  (** [make cmb [f_1; ...; f_n]] creates the formula
      [(f_1 <cmb> ... <cmb> f_n)].*)

  val make_cnf : t -> Literal.LT.t list list
  (** [make_cnf f] returns a conjunctive normal form of [f] under the form: a
      list (which is a conjunction) of lists (which are disjunctions) of
      literals. *)

  val print : Format.formatter -> t -> unit
  (** [print fmt f] prints the formula on the formatter [fmt].*)

end

(** {2 The SMT solver} *)

exception Unsat of int list
(** The exception raised by {! Smt.Solver.check} and {! Smt.Solver.assume} when
    the formula is unsatisfiable. *)

val set_cc : bool -> unit
(** set_cc [false] deactivates congruence closure algorithm
    ([true] by default).*)

module type Solver = sig

  (** This SMT solver is imperative in the sense that it maintains a global
      context. To create different instances of Alt-Ergo Zero use the
      functor {! Smt.Make}.

      A typical use of this solver is to do the following :{[
      clear ();
      assume f_1;
      ...
      assume f_n;
      check ();]}
  *)
    
  type state
  (** The type of the internal state of the solver (see {!save_state} and
      {!restore_state}).*)
    

  (** {2 Profiling functions} *)

  val get_time : unit -> float
  (** [get_time ()] returns the cumulated time spent in the solver in seconds.*)

  val get_calls : unit -> int
  (** [get_calls ()] returns the cumulated number of calls to {! check}.*)

  (** {2 Main API of the solver} *)

  val clear : unit -> unit
  (** [clear ()] clears the context of the solver. Use this after {! check}
      raised {! Unsat} or if you want to restart the solver. *)


  val assume : ?profiling:bool -> id:int -> Formula.t -> unit
  (** [assume ~profiling:b f id] adds the formula [f] to the context of the
      solver with idetifier [id].
      This function only performs unit propagation.
      
      @param profiling if set to [true] then profiling information (time) will
      be computed (expensive because of system calls).
      
      {b Raises} {! Unsat} if the context becomes inconsistent after unit
      propagation. *)

  val check : ?profiling:bool -> unit -> unit
  (** [check ()] runs Alt-Ergo Zero on its context. If [()] is
      returned then the context is satifiable.
      
      @param profiling if set to [true] then profiling information (time) will
      be computed (expensive because of system calls).
      
      {b Raises} {! Unsat} [[id_1; ...; id_n]] if the context is unsatisfiable.
      [id_1, ..., id_n] is the unsat core (returned as the identifiers of the
      formulas given to the solver). *)

  val save_state : unit -> state
  (** [save_state ()] returns a {b copy} of the current state of the solver.*)

  val restore_state : state -> unit
  (** [restore_state s] restores a previously saved state [s].*)

  val entails : ?profiling:bool -> id:int -> Formula.t -> bool
  (** [entails ~id f] returns [true] if the context of the solver entails
      the formula [f]. It doesn't modify the context of the solver (the state
      is saved when this function is called and restored on exit).*)

end

(** Functor to create several instances of the solver *)
module Make (Dummy : sig end) : Solver

