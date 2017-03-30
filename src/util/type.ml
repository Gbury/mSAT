
(** Typechecking of terms from Dolmen.Term.t
    This module defines the requirements for typing expre'ssions from dolmen. *)

module type S = sig

  type atom
  (** The type of atoms that will be fed to tha sovler. *)

  exception Typing_error of string * Dolmen.Term.t
  (** Exception raised during typechecking. *)

  val decl : Dolmen.Id.t -> Dolmen.Term.t -> unit
  (** New declaration, i.e an identifier and its type. *)

  val def  : Dolmen.Id.t -> Dolmen.Term.t -> unit
  (** New definition, i.e an identifier and the term it is equal to. *)

  val assumptions : Dolmen.Term.t -> atom list
  (** Parse a list of local assumptions. *)

  val consequent : Dolmen.Term.t -> atom list list
  val antecedent : Dolmen.Term.t -> atom list list
  (** Parse a formula, and return a cnf ready to be given to the solver.
      Consequent is for hypotheses (left of the sequent), while antecedent
      is for goals (i.e formulas on the right of a sequent). *)

end
