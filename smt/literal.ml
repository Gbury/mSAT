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

type 'a view = 
  | Eq of 'a * 'a 
  | Distinct of bool * 'a list
  | Builtin of bool * Hstring.t * 'a list

module type OrderedType = sig
  type t
  val compare : t -> t -> int
  val hash :  t -> int
  val print : Format.formatter -> t -> unit
end

module type S = sig
  type elt
  type t

  val make : elt view -> t
  val view : t -> elt view

  val neg : t -> t

  val add_label : Hstring.t -> t -> unit
  val label : t -> Hstring.t

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int

  module Map : Map.S with type key = t
  module Set : Set.S with type elt = t
    
end

module Make (X : OrderedType) : S with type elt = X.t = struct

  type elt = X.t
  type t = (X.t view) hash_consed

  module V = struct 
    type t = X.t view 
	
    let equal a1 a2 = 
      match a1, a2 with
	| Eq(t1, t2), Eq(u1, u2) -> 
	    (X.compare t1 u1 = 0 && X.compare t2 u2 = 0) ||
	    (X.compare t1 u2 = 0 && X.compare t2 u1 = 0)
	| Distinct (b1,lt1), Distinct (b2,lt2) ->
	    (try 
	       b1 = b2 && 
		List.for_all2 (fun x y -> X.compare x y = 0) lt1 lt2
	     with Invalid_argument _ -> false)
	| Builtin(b1, n1, l1), Builtin(b2, n2, l2) -> 
	    (try 
	       b1 = b2 && Hstring.equal n1 n2 
		&& 
		List.for_all2 (fun x y -> X.compare x y = 0) l1 l2
	     with Invalid_argument _ -> false)
	| _ -> false
	    
    let hash a = match a with
      | Eq(t1, t2) -> abs (19 * (X.hash t1 + X.hash t2))
      | Distinct (b,lt) ->
	  let x = if b then 7 else 23 in
	  abs (17 * List.fold_left (fun acc t -> (X.hash t) + acc ) x lt)
      | Builtin(b, n, l) -> 
	  let x = if b then 7 else 23 in
	  abs 
	    (List.fold_left 
	       (fun acc t-> acc*13 + X.hash t) (Hstring.hash n+x) l)
  end

  module H = Make_consed(V)

  let compare a1 a2 = Pervasives.compare a1.tag a2.tag
  let equal a1 a2 = a1 == a2
  let hash a1 = a1.tag

  module T = struct 
    type t' = t 
    type t = t' 
    let compare=compare
    let equal = equal
    let hash = hash
  end

  let make t = H.hashcons t
  let view a = a.node

  let neg a = match view a with
    | Eq(x, y) -> make (Distinct (false,[x; y]))
    | Distinct (false, [x; y]) -> make (Eq (x, y))
    | Distinct (true, [x; y]) -> make (Distinct (false,[x; y]))
    | Distinct (false, l) -> make (Distinct (true,l))
    | Distinct _ -> assert false
    | Builtin(b, n, l) -> make (Builtin (not b, n, l))

  module Labels = Hashtbl.Make(T)
    
  let labels = Labels.create 100007
    
  let add_label lbl t = Labels.replace labels t lbl
    
  let label t = try Labels.find labels t with Not_found -> Hstring.empty

  let print_list fmt = function
    | [] -> ()
    | z :: l ->
	Format.fprintf fmt "%a" X.print z;
	List.iter (Format.fprintf fmt ", %a" X.print) l
    
  let ale = Hstring.make "<=" 
  let alt = Hstring.make "<"

  let print fmt a = 
    let lbl = Hstring.view (label a) in
    let lbl = if lbl = "" then lbl else lbl^":" in
    match view a with
      | Eq (z1, z2) -> 
	  if equal z1 z2 then Format.fprintf fmt "True"
	  else Format.fprintf fmt "%s%a=%a" lbl X.print z1 X.print z2
      | Distinct (b,(z::l)) -> 
	  let b = if b then "~" else "" in
	  Format.fprintf fmt "%s%s%a" lbl b X.print z;
	  List.iter (fun x -> Format.fprintf fmt "<>%a" X.print x) l

      | Builtin (true, n, [v1;v2]) when Hstring.equal n ale ->
	  Format.fprintf fmt "%s %a <= %a" lbl X.print v1 X.print v2

      | Builtin (true, n, [v1;v2]) when Hstring.equal n alt ->
	  Format.fprintf fmt "%s %a < %a" lbl X.print v1 X.print v2

      | Builtin (false, n, [v1;v2]) when Hstring.equal n ale ->
	  Format.fprintf fmt "%s %a > %a" lbl X.print v1 X.print v2

      | Builtin (false, n, [v1;v2]) when Hstring.equal n alt ->
	  Format.fprintf fmt "%s %a >= %a" lbl X.print v1 X.print v2

      | Builtin (b, n, l) ->
	  let b = if b then "" else "~" in
	  Format.fprintf fmt "%s%s%s(%a)" lbl b (Hstring.view n) print_list l
      | _ -> assert false
    
  module Set = Set.Make(T)
  module Map = Map.Make(T)

end

module type S_Term = sig

  include S with type elt = Term.t

  val mk_pred : Term.t -> t

  val vrai : t
  val faux : t

(*  val terms_of : t -> Term.Set.t
  val vars_of : t -> Symbols.Set.t
*)
(*  module SetEq : Set.S with type elt = t * Term.t * Term.t*)
end

module LT : S_Term = struct

  module L = Make(Term)
  include L

  let mk_pred t = make (Eq (t, Term.vrai) ) 
    
  let vrai = mk_pred Term.vrai
  let faux = mk_pred Term.faux

  let neg a = match view a with
    | Eq(t1, t2) when Term.equal t2 Term.faux -> 
      make (Eq (t1, Term.vrai))
    | Eq(t1, t2) when Term.equal t2 Term.vrai -> 
      make (Eq (t1, Term.faux))
    | _ -> L.neg a

(* let terms_of a = 
   let l = match view a with 
     | Eq (t1, t2) -> [t1; t2] 
     | Distinct (_, l) | Builtin (_, _, l) -> l 
   in
   List.fold_left Term.subterms Term.Set.empty l
*)

 module SS = Symbols.Set     
(* let vars_of a = 
   Term.Set.fold (fun t -> SS.union (Term.vars_of t)) (terms_of a) SS.empty
*)
end

