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

type symbol = Symbols.t

type t =
  | Tbool
  | Tabstract of ID.t
  | Tsum of ID.t * symbol list

let bool = Tbool
let abstract i = Tabstract i
let sum i l = Tsum (i, l)

let hash t =
  match t with
  | Tabstract s -> ID.hash s
  | Tsum (s, l) ->
    let h =
      List.fold_left
        (fun h x -> 13 * h + Symbols.hash x) (ID.hash s) l
    in
    abs h
  | _ -> Hashtbl.hash t

let equal t1 t2 =
  match t1, t2 with
  | Tabstract s1, Tabstract s2
  | Tsum (s1, _), Tsum (s2, _) ->
    ID.equal s1 s2
  | Tbool, Tbool -> true
  | _ -> false

let compare t1 t2 =
  match t1, t2 with
  | Tabstract s1, Tabstract s2 ->
    ID.compare s1 s2
  | Tabstract _, _ -> -1 | _ , Tabstract _ -> 1
  | Tsum (s1, _), Tsum(s2, _) ->
    ID.compare s1 s2
  | Tsum _, _ -> -1 | _ , Tsum _ -> 1
  | t1, t2 -> Pervasives.compare t1 t2

let print fmt ty =
  match ty with
  | Tbool -> Format.fprintf fmt "bool"
  | Tabstract s -> Format.fprintf fmt "%s" (ID.to_string s)
  | Tsum (s, _) -> Format.fprintf fmt "%s" (ID.to_string s)
