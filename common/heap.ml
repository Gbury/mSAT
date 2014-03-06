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

exception EmptyHeap

module type OrderType = sig
  type t

  val compare : t -> t -> int
end

module type S = sig
  type t
  type elem 
    
  val empty : t
  val pop : t -> elem * t
  val add : t -> elem list -> t
  val elements : t -> elem list
end


module Make ( X : OrderType ) = struct

  type elem = X.t
  type t = Empty | Node of elem * t * t

  let empty = Empty

  let rec fusion t1 t2 = 
    match t1, t2 with
      | _ , Empty -> t1
      | Empty , _ -> t2
      | Node (m1, g1, d1), Node (m2, g2, d2) ->
	  if X.compare m1 m2 <= 0 then Node (m1, d1, fusion g1 t2)
	  else Node (m2, d2, fusion g2 t1)
	    
  let pop = function
    | Empty -> raise EmptyHeap
    | Node(m, g, d) -> m, fusion g d
	
  let add h l = 
    List.fold_left (fun h x -> fusion (Node(x, Empty, Empty)) h ) h l
    
  let elements h = 
    let rec elements_aux acc = function
      | Empty -> acc
      | Node (m1 ,g1 ,d1) -> elements_aux (m1 :: acc) (fusion g1 d1)
    in
    elements_aux [] h

end
