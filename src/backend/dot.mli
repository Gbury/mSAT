(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

(** Dot backend for proofs

    This modules provides functions to export proofs into the dot graph format.
    Graphs in dot format can be used to generates images using the graphviz tool.
*)

module type S = Backend_intf.S
(** Interface for exporting proofs. *)

module type Arg = sig
  (** Term printing for DOT

      This module defines what functions are required in order to export
      a proof to the DOT format.
  *)

  type atom
  (** The type of atomic formuals *)

  type lemma
  (** The type of theory-specifi proofs (also called lemmas). *)

  val print_atom : Format.formatter -> atom -> unit
  (** Print the contents of the given atomic formulas.
      WARNING: this function should take care to esapce and/or not output special
      reserved characters for the dot format (such as quotes and so on). *)

  val lemma_info : lemma -> string * string option * (Format.formatter -> unit -> unit) list
  (** Generate some information about a theory specific lemmas. This backend does not
      support printing of proper proofs in DOT format, so all proofs are printed as leafs
      of the resolution tree. This function should return a triplet [(rule, color, l)],
      such that:
      - [rule] is a name for the proof (arbitrary, does not need to be unique, but
        should rather be descriptive)
      - [color] is a color name (optional) understood by DOT
      - [l] is a list of printers that will be called to print some additional information
  *)

end

module Make(S : Res.S)(A : Arg with type atom := S.atom and type lemma := S.lemma) :
  S with type t := S.proof
(** Functor for making a module to export proofs to the DOT format. *)

