(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

(** Backend interface

    This modules defines the interface of the modules providing
    export of proofs.
*)

module type S = sig
  (** Proof exporting

      Currently, exporting a proof means printing it into a file
      according to the conventions of a given format.
  *)

  type t
  (** The type of proofs. *)

  val print : Format.formatter -> t -> unit
  (** A function for printing proofs in the desired format. *)

end

