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

open Msat

module Sy = Symbols

type view = {
  head: head;
  ty: Ty.t;
  mutable tag: int;
}

and head =
  | App of Symbols.t * t list
  | Ite of t * t * t

and t = view

module H = struct
  type t = view
  let equal t1 t2 = match t1.head, t2.head with
    | App (f1,l1), App (f2,l2) ->
      begin try
        Sy.equal f1 f2
        && List.for_all2 (==) l1 l2
        && Ty.equal t1.ty t2.ty
        with Invalid_argument _ ->
          false
      end
    | Ite (a1,b1,c1), Ite (a2,b2,c2) ->
      a1==a2 && b1==b2 && c1==c2
    | App _, Ite _
    | Ite _, App _ -> false

  let hash t = match t.head with
    | Ite (a,b,c) ->
      abs
        (List.fold_left
           (fun acc x-> acc*19 +x.tag) 17 [a;b;c])
    | App (f,l) ->
      abs
        (List.fold_left
           (fun acc x-> acc*19 +x.tag) (Sy.hash f + Ty.hash t.ty)
           l)
  let set_id x tag = x.tag <- tag
end

module T = Hashcons.Make(H)

let view t = t
let head t = t.head

let rec print fmt t = match t.head with
  | Ite (a,b,c) ->
    Format.fprintf fmt "(@[<2>ite@ %a@ %a@ %a@])" print a print b print c
  | App (x, l) ->
    match x, l with
      | _, [] -> Format.fprintf fmt "%a" Sy.print x
      | _, _ -> Format.fprintf fmt "%a(%a)" Sy.print x print_list l

and print_list fmt = function
  | [] -> ()
  | [t] -> print fmt t
  | t::l -> Format.fprintf fmt "%a,%a" print t print_list l

let compare t1 t2 =
  let c = Pervasives.compare t2.tag t1.tag in
  if c = 0 then c
  else match t1.head, t2.head with
    | App ((Sy.True | Sy.False), _), App ((Sy.True | Sy.False), _) -> c
    | App ((Sy.True | Sy.False), _), _ -> -1
    | _, App ((Sy.True | Sy.False), _) -> 1
    | _,_ -> c

let app s l ty = T.hashcons {head=App(s,l); ty=ty; tag= -1; }
let const s ty = app s [] ty
let ite a b c = T.hashcons {head=Ite (a,b,c); ty=b.ty; tag= -1; }

let true_ = app (Sy.True) [] Ty.Tbool
let false_ = app (Sy.False) [] Ty.Tbool

let equal t1 t2 =  t1 == t2

let hash t = t.tag

module Set =
  Set.Make(struct type t' = t type t=t' let compare=compare end)

module Map =
  Map.Make(struct type t' = t type t=t' let compare=compare end)
