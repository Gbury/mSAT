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

open Format
open Hashcons

module Sy = Symbols

type view = {f: Sy.t ; xs: t list; ty: Ty.t; tag: int}
and t = view

module H = struct
  type t = view
  let equal t1 t2 = try
    Sy.equal t1.f t2.f 
    && List.for_all2 (==) t1.xs t2.xs 
    && Ty.equal t1.ty t2.ty
  with Invalid_argument _ -> false
      
  let hash t =
    abs (List.fold_left 
	   (fun acc x-> acc*19 +x.tag) (Sy.hash t.f + Ty.hash t.ty) 
	   t.xs)
  let tag tag x = {x with tag = tag}
end

module T = Make(H)
  
let view t = t

let rec print fmt t = 
  let {f=x; xs=l; ty=ty} = view t in
  match x, l with
    | Sy.Op op, [e1; e2] -> 
	fprintf fmt "(%a %a %a)" print e1 Sy.print x print e2
    | _, [] -> fprintf fmt "%a" Sy.print x
    | _, _ -> fprintf fmt "%a(%a)" Sy.print x print_list l

and print_list fmt = function
  | [] -> ()
  | [t] -> print fmt t
  | t::l -> Format.fprintf fmt "%a,%a" print t print_list l

let compare t1 t2 =
  let c = Pervasives.compare t2.tag t1.tag in
  if c = 0 then c else
  match (view t1).f, (view t2).f with
    | (Sy.True | Sy.False ), (Sy.True | Sy.False ) -> c
    | (Sy.True | Sy.False ), _ -> -1
    | _, (Sy.True | Sy.False ) -> 1
    | _,_ -> c

let make s l ty = T.hashcons {f=s;xs=l;ty=ty;tag=0 (* dumb_value *) }

let vrai = make (Sy.True) [] Ty.Tbool
let faux = make (Sy.False) [] Ty.Tbool

let int i = make (Sy.int i) [] Ty.Tint
let real r = make (Sy.real r) [] Ty.Treal

let is_int t = (view t).ty= Ty.Tint
let is_real t = (view t).ty= Ty.Treal

let equal t1 t2 =  t1 == t2
  
let hash t = t.tag
  
module Set = 
  Set.Make(struct type t' = t type t=t' let compare=compare end)
    
module Map = 
  Map.Make(struct type t' = t type t=t' let compare=compare end)
