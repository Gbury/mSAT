
(** Provides helpers for backtracking.
    This module defines backtracking stacks, i.e stacks of undo actions
    to perform when backtracking to a certain point. This allows for
    side-effect backtracking, and so to have backtracking automatically
    handled by extensions without the need for explicit synchronisation
    between the dispatcher and the extensions.
*)

module Stack : sig
  (** A backtracking stack is a stack of undo actions to perform
      in order to revert back to a (mutable) state. *)

  type t
  (** The type for a stack. *)

  type level
  (** The type of backtracking point. *)

  val create : unit -> t
  (** Creates an empty stack. *)

  val dummy_level : level
  (** A dummy level. *)

  val push : t -> unit
  (** Creates a backtracking point at the top of the stack. *)

  val pop : t -> unit
  (** Pop all actions in the undo stack until the first backtracking point. *)

  val level : t -> level
  (** Insert a named backtracking point at the top of the stack. *)

  val backtrack : t -> level -> unit
  (** Backtrack to the given named backtracking point. *)

  val register_undo : t -> (unit -> unit) -> unit
  (** Adds a callback at the top of the stack. *)

  val register1 : t -> ('a -> unit) -> 'a -> unit
  val register2 : t -> ('a -> 'b -> unit) -> 'a -> 'b -> unit
  val register3 : t -> ('a -> 'b -> 'c -> unit) -> 'a -> 'b -> 'c -> unit
  (** Register functions to be called on the given arguments at the top of the stack.
      Allows to save some space by not creating too much closure as would be the case if
      only [unit -> unit] callbacks were stored. *)

  val register_set : t -> 'a ref -> 'a -> unit
  (** Registers a ref to be set to the given value upon backtracking. *)

end

module Hashtbl :
  functor (K : Hashtbl.HashedType) ->
  sig
    (** Provides wrappers around hastables in order to have
        very simple integration with backtraking stacks.
        All actions performed on this table register the corresponding
        undo operations so that backtracking is automatic. *)

    type key = K.t
    (** The type of keys of the Hashtbl. *)

    type 'a t
    (** The type of hastable from keys to values of type ['a]. *)

    val create : ?size:int -> Stack.t -> 'a t
    (** Creates an empty hashtable, that registers undo operations on the given stack. *)

    val add : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val find : 'a t -> key -> 'a
    val remove : 'a t -> key -> unit
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : 'a t -> (key -> 'a -> 'b -> 'b) -> 'b -> 'b
    (** Usual operations on the hashtabl. For more information see the Hashtbl module of the stdlib. *)
  end
