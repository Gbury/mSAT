
(* This file is free software, part of mSAT. See file "LICENSE" for more information. *)

open Ocamlbuild_plugin;;

let doc_intro = "src/doc.txt";;

dispatch begin function
  | After_rules ->
    (* Documentation index *)
    dep ["ocaml"; "doc"; "extension:html"] & [doc_intro] ;
    flag ["ocaml"; "doc"; "extension:html"]
    & S [ A "-t"; A "mSAT doc"; A "-intro"; P doc_intro ];
  | _ -> ()
end

