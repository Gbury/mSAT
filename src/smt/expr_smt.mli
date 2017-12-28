
(** Expressions for TabSat *)

(** {2 Type definitions} *)

(** These are custom types used in functions later. *)

(** {3 Identifiers} *)

(** Identifiers are the basic building blocks used to build types terms and expressions. *)

type hash
type index = private int

(** Private aliases to provide access. You should not have any need
    to use these, instead use the functions provided by this module. *)

type 'ty id = private {
  id_type : 'ty;
  id_name : string;
  index   : index; (** unique *)
}

(** The type of identifiers. An ['a id] is an identifier whose solver-type
    is represented by an inhabitant of type ['a].
    All identifier have an unique [index] which is used for comparison,
    so that the name of a variable is only there for tracability
    and/or pretty-printing. *)

(** {3 Types} *)

type ttype = Type

(** The caml type of solver-types. *)

type 'ty function_descr = private {
  fun_vars : ttype id list; (* prenex forall *)
  fun_args : 'ty list;
  fun_ret : 'ty;
}

(** This represents the solver-type of a function.
    Functions can be polymorphic in the variables described in the
    [fun_vars] field. *)

type ty_descr = private
  | TyVar of ttype id
  (** bound variables (i.e should only appear under a quantifier) *)
  | TyApp of ttype function_descr id * ty list
  (** application of a constant to some arguments *)

and ty = private {
  ty : ty_descr;
  mutable ty_hash : hash; (** Use Ty.hash instead *)
}

(** These types defines solver-types, i.e the representation of the types
    of terms in the solver. Record definition for [type ty] is shown in order
    to be able to use the [ty.ty] field in patter matches. Other fields shoud not
    be accessed directly, but throught the functions provided by the [Ty] module. *)

(** {3 Terms} *)

type term_descr = private
  | Var of ty id
  (** bound variables (i.e should only appear under a quantifier) *)
  | App of ty function_descr id * ty list * term list
  (** application of a constant to some arguments *)

and term = private {
  term     : term_descr;
  t_type   : ty;
  mutable t_hash : hash; (** Do not use this filed, call Term.hash instead *)
}

(** Types defining terms in the solver. The definition is vary similar to that
    of solver-types, except for type arguments of polymorphic functions which
    are explicit. This has the advantage that there is a clear and typed distinction
    between solver-types and terms, but may lead to some duplication of code
    in some places. *)

(** {3 Formulas} *)

type atom_descr = private
  (** Atoms *)
  | Pred of term
  | Equal of term * term

and atom = private {
  sign : bool;
  atom : atom_descr;
  mutable f_hash : hash; (** Use Formula.hash instead *)
}

(** The type of atoms in the solver. The list of free arguments in quantifiers
    is a bit tricky, so you should not touch it (see full doc for further
    explanations). *)

(** {3 Exceptions} *)

exception Type_mismatch of term * ty * ty
(* Raised when as Type_mismatch(term, actual_type, expected_type) *)

exception Bad_arity of ty function_descr id * ty list * term list
exception Bad_ty_arity of ttype function_descr id * ty list
(** Raised when trying to build an application with wrong arity *)

(** {2 Printing} *)

module Print : sig
  (** Pretty printing functions *)

  val id : Format.formatter -> 'a id -> unit
  val id_ty : Format.formatter -> ty id -> unit
  val id_ttype : Format.formatter -> ttype id -> unit

  val const_ty : Format.formatter -> ty function_descr id -> unit
  val const_ttype : Format.formatter -> ttype function_descr id -> unit

  val ty : Format.formatter -> ty -> unit
  val fun_ty : Format.formatter -> ty function_descr -> unit

  val ttype : Format.formatter -> ttype -> unit
  val fun_ttype : Format.formatter -> ttype function_descr -> unit

  val term : Format.formatter -> term -> unit
  val atom : Format.formatter -> atom -> unit
end

(** {2 Identifiers & Metas} *)

module Id : sig
  type 'a t = 'a id
  (* Type alias *)

  val hash : 'a t -> int
  val equal : 'a t -> 'a t -> bool
  val compare : 'a t -> 'a t -> int
  (** Usual functions for hash/comparison *)

  val print : Format.formatter -> 'a t -> unit
  (** Printing for variables *)

  val prop : ttype function_descr id
  val base : ttype function_descr id
  (** Constants representing the type for propositions and a default type
      for term, respectively. *)

  val ttype : string -> ttype id
  (** Create a fresh type variable with the given name. *)

  val ty : string -> ty -> ty id
  (** Create a fresh variable with given name and type *)

  val ty_fun : string -> int -> ttype function_descr id
  (** Create a fresh type constructor with given name and arity *)

  val term_fun : string -> ttype id list -> ty list -> ty -> ty function_descr id
  (** [ty_fun name type_vars arg_types return_type] returns a fresh constant symbol,
      possibly polymorphic with respect to the variables in [type_vars] (which may appear in the
      types in [arg_types] and in [return_type]). *)

end

(** {2 Substitutions} *)

module Subst : sig
  (** Module to handle substitutions *)

  type ('a, 'b) t
  (** The type of substitutions from values of type ['a] to values of type ['b]. *)

  val empty : ('a, 'b) t
  (** The empty substitution *)

  val is_empty : ('a, 'b) t -> bool
  (** Test wether a substitution is empty *)

  val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
  (** Iterates over the bindings of the substitution. *)

  val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
  (** Fold over the elements *)

  val bindings : ('a, 'b) t -> ('a * 'b) list
  (** Returns the list of bindings ofa substitution. *)

  val exists : ('a -> 'b -> bool) -> ('a, 'b) t -> bool
  (** Tests wether the predicate holds for at least one binding. *)

  val for_all : ('a -> 'b -> bool) -> ('a, 'b) t -> bool
  (** Tests wether the predicate holds for all bindings. *)

  val hash : ('b -> int) -> ('a, 'b) t -> int
  val compare : ('b -> 'b -> int) -> ('a, 'b) t -> ('a, 'b) t -> int
  val equal : ('b -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool
  (** Comparison and hash functions, with a comparison/hash function on values as parameter *)

  val print :
    (Format.formatter -> 'a -> unit) ->
    (Format.formatter -> 'b -> unit) ->
    Format.formatter -> ('a, 'b) t -> unit
  (** Prints the substitution, using the given functions to print keys and values. *)

  val choose : ('a, 'b) t -> 'a * 'b
  (** Return one binding of the given substitution, or raise Not_found if the substitution is empty.*)

  (** {5 Concrete subtitutions } *)
  module type S = sig
    type 'a key
    val get : 'a key -> ('a key, 'b) t -> 'b
    (** [get v subst] returns the value associated with [v] in [subst], if it exists.
        @raise Not_found if there is no binding for [v]. *)
    val mem : 'a key -> ('a key, 'b) t -> bool
    (** [get v subst] returns wether there is a value associated with [v] in [subst]. *)
    val bind : 'a key -> 'b -> ('a key, 'b) t -> ('a key, 'b) t
    (** [bind v t subst] returns the same substitution as [subst] with the additional binding from [v] to [t].
        Erases the previous binding of [v] if it exists. *)
    val remove : 'a key -> ('a key, 'b) t -> ('a key, 'b) t
    (** [remove v subst] returns the same substitution as [subst] except for [v] which is unbound in the returned substitution. *)
  end

  module Id : S with type 'a key = 'a id
end

(** {2 Types} *)

module Ty : sig
  type t = ty
  (** Type alias *)

  type subst = (ttype id, ty) Subst.t
  (** The type of substitutions over types. *)

  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  (** Usual hash/compare functions *)

  val print : Format.formatter -> t -> unit

  val prop : ty
  val base : ty
  (** The type of propositions and individuals *)

  val of_id : ttype id -> ty
  (** Creates a type from a variable *)

  val apply : ttype function_descr id -> ty list -> ty
  (** Applies a constant to a list of types *)

  val subst : subst -> ty -> ty
  (** Substitution over types. *)

end

(** {2 Terms} *)

module Term : sig
  type t = term
  (** Type alias *)

  type subst = (ty id, term) Subst.t
  (** The type of substitutions in types. *)

  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  (**  Usual hash/compare functions *)

  val print : Format.formatter -> t -> unit
  (** Printing functions *)

  val of_id : ty id -> term
  (** Create a term from a variable *)

  val apply : ty function_descr id -> ty list -> term list -> term
  (** Applies a constant function to type arguments, then term arguments *)

  val subst : Ty.subst -> subst -> term -> term
  (** Substitution over types. *)

  val replace : term * term -> term -> term
  (** [replace (t, t') t''] returns the term [t''] where every occurence of [t]
      has been replace by [t']. *)

end

(** {2 Formulas} *)

module Atom : sig
  type t = atom
  type proof = unit
  (** Type alias *)

  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  (** Usual hash/compare functions *)

  val print : Format.formatter -> t -> unit
  (** Printing functions *)

  val dummy : atom
  (** A dummy atom, different from any other atom. *)

  val fresh : unit -> atom
  (** Create a fresh propositional atom. *)

  val eq : term -> term -> atom
  (** Create an equality over two terms. The two given terms
      must have the same type [t], which must be different from {!Ty.prop} *)

  val pred : term -> atom
  (** Create a atom from a term. The given term must have type {!Ty.prop} *)

  val neg : atom -> atom
  (** Returns the negation of the given atom *)

  val norm : atom -> atom * Formula_intf.negated
  (** Normalization functions as required by msat. *)

end

module Formula : Msat_solver.Tseitin.S with type atom = atom

