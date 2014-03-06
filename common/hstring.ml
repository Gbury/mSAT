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

module S = 
  Hashcons.Make_consed(struct include String 
		       let hash = Hashtbl.hash 
		       let equal = (=)     end)

module HS = struct

  type t = string Hashcons.hash_consed

  let make s = S.hashcons s

  let view s = s.node

  let equal s1 s2 = s1.tag = s2.tag

  let compare s1 s2 = compare s1.tag s2.tag

  let hash s = s.tag

  let empty = make ""

  let rec list_assoc x = function
    | [] -> raise Not_found
    | (y, v) :: l -> if equal x y then v else list_assoc x l

  let rec list_mem_assoc x = function 
    | [] -> false
    | (y, _) :: l -> compare x y = 0 || list_mem_assoc x l

  let rec list_mem x = function
    | [] -> false
    | y :: l -> compare x y  = 0 || list_mem x l

  let compare_couple (x1,y1) (x2,y2) =
    let c = compare x1 x2 in
    if c <> 0 then c
    else compare y1 y2

  let rec compare_list l1 l2 =
    match l1, l2 with
      | [], [] -> 0
      | [], _ -> -1
      | _, [] -> 1
      | x::r1, y::r2 ->
	let c = compare x y in
	if c <> 0 then c
	else compare_list r1 r2

  let rec list_mem_couple c = function
    | [] -> false
    | d :: l -> compare_couple c d  = 0 || list_mem_couple c l

  let print fmt s = 
    Format.fprintf fmt "%s" (view s)

end

include HS

module H = Hashtbl.Make(HS)

module HSet = Set.Make(HS)

module HMap = Map.Make(HS)

(* struct *)
(*   include Hashtbl.Make(HS) *)

(*   let find x h = *)
(*     TimeHS.start (); *)
(*     try      *)
(*       let r = find x h in *)
(*       TimeHS.pause (); *)
(*       r *)
(*     with Not_found -> TimeHS.pause (); raise Not_found *)
(* end *)
