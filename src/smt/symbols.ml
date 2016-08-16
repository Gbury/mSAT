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

type name_kind = Constructor | Other

type t =
  | True
  | False
  | Name of ID.t * name_kind
  | Var of ID.t

let name ?(kind=Other) s = Name (s, kind)
let var s = Var s

let compare_kind k1 k2 = match k1, k2 with
  | Other, Other -> 0
  | Other, _     -> 1
  | _    , Other -> -1
  | Constructor, Constructor -> 0

let compare s1 s2 =  match s1, s2 with
  | Name (n1,k1), Name (n2,k2) ->
    let c = compare_kind k1 k2 in
    if c = 0 then ID.compare n1 n2 else c
  | Name _, _ ->  -1
  | _, Name _ -> 1
  | Var n1, Var n2 -> ID.compare n1 n2
  | Var _, _ -> -1
  | _ ,Var _ -> 1
  | _  -> Pervasives.compare s1 s2

let equal s1 s2 = compare s1 s2 = 0

let hash = function
  | Name (n,_) -> ID.hash n * 19
  | Var n (*| Int n*) -> ID.hash n * 19 + 1
  | s -> Hashtbl.hash s

let to_string =  function
  | Name (n,_) -> ID.to_string n
  | Var x -> "*var* "^(ID.to_string x)
  | True -> "true"
  | False -> "false"

let print fmt s = Format.fprintf fmt "%s" (to_string s)

module Map =
  Map.Make(struct type t' = t type t=t' let compare=compare end)

module Set =
  Set.Make(struct type t' = t type t=t' let compare=compare end)

