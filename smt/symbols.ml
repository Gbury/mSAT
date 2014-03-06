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

open Hashcons

type operator = 
  | Plus | Minus | Mult | Div | Modulo 

type name_kind = Ac | Constructor | Other

type t = 
  | True 
  | False
  | Name of Hstring.t * name_kind
  | Int of Hstring.t
  | Real of Hstring.t
  | Op of operator
  | Var of Hstring.t

let name ?(kind=Other) s = Name (s, kind)
let var s = Var (Hstring.make s)
let int i = Int (Hstring.make i)
let real r = Real (Hstring.make r)


let is_ac = function
  | Name(_, Ac) -> true
  | _           -> false

let compare_kind k1 k2 = match k1, k2 with
  | Ac   , Ac    -> 0
  | Ac   , _     -> 1
  | _    , Ac    -> -1
  | Other, Other -> 0
  | Other, _     -> 1
  | _    , Other -> -1
  | Constructor, Constructor -> 0

let compare s1 s2 =  match s1, s2 with
  | Name (n1,k1), Name (n2,k2) -> 
      let c = compare_kind k1 k2 in
      if c = 0 then Hstring.compare n1 n2 else c
  | Name _, _ ->  -1
  | _, Name _ -> 1
  | Var n1, Var n2 -> Hstring.compare n1 n2
  | Var _, _ -> -1
  | _ ,Var _ -> 1
  | Int i1, Int i2 -> Hstring.compare i1 i2
  | Int _, _ -> -1
  | _ ,Int _ -> 1
  | _  -> Pervasives.compare s1 s2
  
let equal s1 s2 = compare s1 s2 = 0

let hash = function
  | Name (n,Ac) -> Hstring.hash n * 19 + 1
  | Name (n,_) -> Hstring.hash n * 19
  | Var n (*| Int n*) -> Hstring.hash n * 19 + 1
  | s -> Hashtbl.hash s
	
let to_string =  function
  | Name (n,_) -> Hstring.view n
  | Var x -> "*var* "^(Hstring.view x)
  | Int n -> Hstring.view n
  | Real n -> Hstring.view n
  | Op Plus -> "+" 
  | Op Minus -> "-" 
  | Op Mult -> "*"
  | Op Div -> "/"
  | Op Modulo -> "%"
  | True -> "true"
  | False -> "false"

let print fmt s = Format.fprintf fmt "%s" (to_string s)

module Map =
  Map.Make(struct type t' = t type t=t' let compare=compare end)

module Set = 
  Set.Make(struct type t' = t type t=t' let compare=compare end)

