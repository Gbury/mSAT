(**************************************************************************)
(*                                                                        *)
(*                                  Cubicle                               *)
(*             Combining model checking algorithms and SMT solvers        *)
(*                                                                        *)
(*                  Sylvain Conchon, Evelyne Contejean                    *)
(*                  Francois Bobot, Mohamed Iguernelala, Alain Mebsout    *)
(*                  CNRS, Universite Paris-Sud 11                         *)
(*                                                                        *)
(*  Copyright 2011. This file is distributed under the terms of the       *)
(*  Apache Software License version 2.0                                   *)
(*                                                                        *)
(**************************************************************************)

module F = Format
module L = List
module T = Term
module S = Symbols
module ST = T.Set
(* module SA = Literal.LT.Set *)

module SA = Set.Make(struct
  type t = Literal.LT.t * Explanation.t
  let compare (s1,_) (s2,_) = Literal.LT.compare s1 s2
end)

type elt = ST.t * SA.t
  
module Make (X : Sig.X) = struct

  let inter_tpl (x1,y1) (x2,y2) = ST.inter x1 x2, SA.inter y1 y2
  let union_tpl (x1,y1) (x2,y2) = ST.union x1 x2, SA.union y1 y2
  let bottom = Hstring.make "@bottom" 
  let leaves r = 
    let one, _ = X.make (T.make (Symbols.name bottom) [] Ty.Tint) in
    match X.leaves r with [] -> [one] | l -> l

  module G = Map.Make(struct type t = X.r include X end)

  open G

  type t = elt G.t

      
  let find k m = try find k m with Not_found -> (ST.empty,SA.empty)
      
  let add_term k t mp =
    let g_t,g_a = find k mp in add k (ST.add t g_t,g_a) mp
				 
  let up_add g t rt lvs = 
    let g = if mem rt g then g else add rt (ST.empty, SA.empty) g in
    L.fold_left (fun g x -> add_term x t g) g lvs 
      
  let congr_add g lvs = 
    match lvs with
	[]    -> ST.empty
      | x::ls -> 
	  L.fold_left 
	    (fun acc y -> ST.inter (fst(find y g)) acc)
	    (fst(find x g)) ls
	    
  let up_close_up g p v = 
    let lvs = leaves v in
    let g_p = find p g in
    L.fold_left (fun gg l -> add l (union_tpl g_p (find l g)) gg) g lvs
      
  let congr_close_up g p touched =
    let inter = function 
	[] -> (ST.empty, SA.empty)
      | rx::l -> 
	  L.fold_left (fun acc x ->inter_tpl acc (find x g))(find rx g) l
    in 
    L.fold_left 
      (fun (st,sa) tch -> union_tpl (st,sa)(inter (leaves tch)))
      (find p g) touched 
      
  let print g = ()

  let mem = G.mem
  let add = G.add
  let empty = G.empty

end
