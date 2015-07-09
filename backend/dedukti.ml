(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module type S = Backend_intf.S

module Make(S : Res.S)(A : Backend_intf.Arg with type formula := S.atom and type proof := S.proof) = struct

  let print_aux fmt = Format.fprintf fmt "@\n"

  let fprintf fmt format = Format.kfprintf print_aux fmt format

  let context fmt () =
    fprintf fmt "(; Embedding ;)";
    fprintf fmt "Prop : Type.";
    fprintf fmt "_proof : Prop -> Type.";
    fprintf fmt "(; Notations for clauses ;)";
    fprintf fmt "_pos : Prop -> Prop -> Type.";
    fprintf fmt "_neg : Prop -> Prop -> Type.";
    fprintf fmt "[b: Prop, p: Prop] _pos b p --> _proof p -> _proof b.";
    fprintf fmt "[b: Prop, p: Prop] _neg b p --> _pos b p -> _proof b."

  let print fmt _ =
    context fmt ();
    ()

end

