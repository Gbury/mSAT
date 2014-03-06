(**************************************************************************)
(*                                                                        *)
(*                                  Cubicle                               *)
(*             Combining model checking algorithms and SMT solvers        *)
(*                                                                        *)
(*                  Sylvain Conchon, Alain Mebsout                        *)
(*                  Francois Bobot, Mohamed Iguernelala                   *)
(*                  Universite Paris-Sud 11                               *)
(*                                                                        *)
(*  Copyright 2011. This file is distributed under the terms of the       *)
(*  Apache Software License version 2.0                                   *)
(*                                                                        *)
(**************************************************************************)

open Format
open Sig
open Num
module A = Literal
module Sy = Symbols
module T = Term

let ale = Hstring.make "<=" 
let alt = Hstring.make "<"
let is_le n = Hstring.compare n ale = 0
let is_lt n = Hstring.compare n alt = 0
let is_mult h = Sy.equal (Sy.Op Sy.Mult) h
let mod_symb = Sy.name (Hstring.make "@mod")

module Type (X:Sig.X) : Polynome.T with type r =  X.r = struct

  let mult _ _ = assert false

  include Polynome.Make(struct include X let mult = mult end)

end

module Make 
  (X : Sig.X)
  (P : Polynome.T with type r = X.r)
  (C : Sig.C with type t = P.t and type r = X.r) = struct

  type t = P.t

  type r = P.r
  
  let name = "arith"

  let is_mine_a a = 
    match A.LT.view a with
      | A.Builtin (_,p,_) -> is_le p || is_lt p
      | _ -> false

  let is_mine_symb = function
    | Sy.Int _ | Sy.Real _ 
    | Sy.Op (Sy.Plus | Sy.Minus | Sy.Mult | Sy.Div | Sy.Modulo) -> true
    | _ -> false

  let is_mine_type p = 
    let ty = P.type_info p in 
    ty = Ty.Tint || ty = Ty.Treal

  let unsolvable _ = false
	  
  let empty_polynome ty = P.create [] (Int 0) ty

  let is_mine p = match P.is_monomial p with
    | Some (a,x,b) when a =/ (Int 1) && b =/ (Int 0) -> x
    | _ -> C.embed p
    
  let embed r = match C.extract r with
    | Some p -> p
    | _ -> P.create [Int 1, r] (Int 0) (X.type_info r)  

  let check_int exn p =  
    if P.type_info p = Ty.Tint then
      let _, c = P.to_list p in
      let ppmc = P.ppmc_denominators p in
      if not (is_integer_num (ppmc */ c)) then raise exn
      
  let fresh_string = 
    let cpt = ref 0 in
    fun () ->
      incr cpt;
      "!k" ^ (string_of_int !cpt)

  let fresh_name () = 
    T.make (Sy.name (Hstring.make (fresh_string()))) [] Ty.Tint

  (* t1 % t2 = md  <-> 
     c1. 0 <= md ;
     c2. md < t2 ;
     c3. exists k. t1 = t2 * k + t ;
     c4. t2 <> 0 (already checked) *)
  let mk_modulo md t1 t2 ctx = 
    let zero = T.int "0" in
    let c1 = A.LT.make (A.Builtin(true, ale, [zero; md])) in
    let c2 = A.LT.make (A.Builtin(true, alt, [md; t2])) in
    let k  = fresh_name () in
    let t3 = T.make (Sy.Op Sy.Mult) [t2;k] Ty.Tint in
    let t3 = T.make (Sy.Op Sy.Plus) [t3;md] Ty.Tint in
    let c3 = A.LT.make (A.Eq (t1, t3)) in
    c3 :: c2 :: c1 :: ctx    

  let mk_euc_division p p2 t1 t2 ctx = 
    match P.to_list p2 with
      | [], coef_p2 ->
          let md = T.make (Sy.Op Sy.Modulo) [t1;t2] Ty.Tint in
          let r, ctx' = X.make md in
          let rp = P.mult (P.create [] ((Int 1) //coef_p2) Ty.Tint) (embed r) in
          P.sub p rp, ctx' @ ctx
      | _ -> assert false

  let rec mke coef p t ctx =
    let {T.f = sb ; xs = xs; ty = ty} = T.view t in
    match sb, xs with
      | (Sy.Int n | Sy.Real n), _  -> 
	  let c = coef */ (num_of_string (Hstring.view n)) in
	  P.add (P.create [] c ty) p, ctx

      | Sy.Op Sy.Mult, [t1;t2] ->
	  let p1, ctx = mke coef (empty_polynome ty) t1 ctx in
	  let p2, ctx = mke (Int 1) (empty_polynome ty) t2 ctx in
	  P.add p (P.mult p1 p2), ctx

      | Sy.Op Sy.Div, [t1;t2] -> 
	  let p1, ctx = mke coef (empty_polynome ty) t1 ctx in
	  let p2, ctx = mke (Int 1) (empty_polynome ty) t2 ctx in
	  let p3, ctx = 
	    try 
              let p, approx = P.div p1 p2 in
              if approx then mk_euc_division p p2 t1 t2 ctx
              else p, ctx
	    with Division_by_zero | Polynome.Maybe_zero -> 
              P.create [coef, X.term_embed t] (Int 0) ty, ctx
	  in
	  P.add p p3, ctx
		
      | Sy.Op Sy.Plus , [t1;t2] -> 
	  let p2, ctx = mke coef p t2 ctx in
	  mke coef p2 t1 ctx

      | Sy.Op Sy.Minus , [t1;t2] -> 
	  let p2, ctx = mke (minus_num coef) p t2 ctx in
	  mke coef p2 t1 ctx

      | Sy.Op Sy.Modulo , [t1;t2] -> 
	  let p1, ctx = mke coef (empty_polynome ty) t1 ctx in
	  let p2, ctx = mke (Int 1) (empty_polynome ty) t2 ctx in
          let p3, ctx = 
            try P.modulo p1 p2, ctx
            with e ->
	      let t = T.make mod_symb [t1; t2] Ty.Tint in    
              let ctx = match e with
                | Division_by_zero | Polynome.Maybe_zero -> ctx
                | Polynome.Not_a_num -> mk_modulo t t1 t2 ctx
                | _ -> assert false 
              in 
              P.create [coef, X.term_embed t] (Int 0) ty, ctx 
	  in         
	  P.add p p3, ctx
	    
      | _ ->
	let a, ctx' = X.make t in
	let ctx = ctx' @ ctx in
	match C.extract a with
	  | Some p' -> P.add p (P.mult (P.create [] coef ty) p'), ctx
	  | _ -> P.add p (P.create [coef, a] (Int 0) ty), ctx

  let arith_to_ac p = p
(*
    match P.to_list p with
      | []         , c     -> p
      | [Int 1, x] , Int 0 -> p
      | l          , c     ->
        let ty = P.type_info p in
        let l = 
          List.fold_left 
            (fun acc (coef,x) ->
              if coef =/ Int 0 then acc
              else if coef =/ Int 1 || coef =/ Int (-1) then (coef,x)::acc
              else match X.ac_extract x with
                | Some ac when is_mult ac.h ->
                  let unit_coef, abs_coef =
                    if coef > Int 0 then Int 1, coef
                    else Int (-1), minus_num coef 
                  in
                  let p_cst = is_mine (P.create [] abs_coef ty) in
                  let ac = {ac with l = Ac.add ac.h (p_cst, 1) ac.l} in
                  (unit_coef, X.ac_embed ac)::acc
                | _      -> (coef,x)::acc
            )[] l 
        in 
        P.create l c ty
*)
  let make t =
    let {T.ty = ty} = T.view t in
    let p, ctx = mke (Int 1) (empty_polynome ty) t [] in
    is_mine (arith_to_ac p), ctx

  let rec expand p n acc =
    assert (n >=0);
    if n = 0 then acc else expand p (n-1) (p::acc)

  let rec number_of_vars l = 
    List.fold_left (fun acc (r, n) -> acc + n * nb_vars_in_alien r) 0 l 

  and nb_vars_in_alien r = 
    match C.extract r with
      | Some p -> 
	  let l, _ = P.to_list p in
          List.fold_left (fun acc (a, x) -> max acc (nb_vars_in_alien x)) 0 l
      | None -> 1

  let max_list_ = function
    | [] -> 0
    | [ _, x ] -> nb_vars_in_alien x
    | (_, x) :: l ->
	let acc = nb_vars_in_alien x in
	List.fold_left (fun acc (_, x) -> max acc (nb_vars_in_alien x)) acc l

  let type_info p = P.type_info p

  let is_int r = X.type_info r = Ty.Tint

  module XS = Set.Make(struct type t = X.r let compare = X.compare end)
    
  let xs_of_list = 
    List.fold_left (fun s x -> XS.add x s) XS.empty
      
  let rec leaves p = 
    let s = 
      List.fold_left
	(fun s (_, a) -> XS.union (xs_of_list (X.leaves a)) s)
	XS.empty (fst (P.to_list p))
    in
    XS.elements s

  let subst x t p = 
    let p = P.subst x (embed t) p in
    let ty = P.type_info p in
    let l, c = P.to_list p in
    let p  = 
      List.fold_left
        (fun p (ai, xi) ->
	   let xi' = X.subst x t xi in
	   let p' = match C.extract xi' with
	     | Some p' -> P.mult (P.create [] ai ty) p'
	     | _ -> P.create [ai, xi'] (Int 0) ty
	   in
	   P.add p p')
        (P.create [] c ty) l
    in 
    check_int (Exception.Unsolvable) p;
    is_mine p


  let compare = P.compare

  let hash = P.hash

  (* symmetric modulo p 131 *)
  let mod_sym a b = 
    let m = mod_num a b in 
    let m = 
      if m </ Int 0 then
        if m >=/ minus_num b then m +/ b else assert false
      else 
        if m <=/ b then m else assert false
	  
    in
    if m </ b // (Int 2) then m else m -/ b

  let mult_const p c =
    P.mult p (P.create [] c (P.type_info p))
  
  let map_monomes f l ax =
    List.fold_left
      (fun acc (a,x) -> 
         let a = f a in if a =/ Int 0 then acc else (a, x) :: acc)
      [ax] l 

  let apply_subst sb v = 
    is_mine (List.fold_left (fun v (x, p) -> embed (subst x p v)) v sb)

  (* substituer toutes variables plus grandes que x *)
  let subst_bigger x l = 
    List.fold_left 
      (fun (l, sb) (b, y) ->
         if X.compare y x > 0 then
	   let k = X.term_embed (fresh_name ()) in
	   (b, k) :: l, (y, embed k)::sb
	 else (b, y) :: l, sb)
      ([], []) l

  let is_mine_p = List.map (fun (x,p) -> x, is_mine p)
   
  let extract_min = function
      | [] -> assert false
      | [c] -> c, []
      | (a, x) :: s -> 
	  List.fold_left 
	     (fun ((a, x), l) (b, y) ->
		if abs_num a <=/ abs_num b then 
		  (a, x), ((b, y) :: l) 
		else (b, y), ((a, x):: l)) ((a, x),[]) s
      

  (* Decision Procedures. Page 131 *)
  let rec omega l b = 
    
    (* 1. choix d'une variable donc le |coef| est minimal *)
    let (a, x), l = extract_min l in 

    (* 2. substituer les aliens plus grand que x pour 
       assurer l'invariant sur l'ordre AC *)
    let l, sbs = subst_bigger x l in
    let p = P.create l b Ty.Tint in
    match a with
      | Int 0 -> assert false
      | Int 1 -> 
          (* 3.1. si a = 1 alors on a une substitution entiere pour x *)
          let p = mult_const p (Int (-1)) in 
          (x, is_mine p) :: (is_mine_p sbs)
            
      | Int (-1) -> 
          (* 3.2. si a = -1 alors on a une subst entiere pour x*)
          (x,is_mine p) :: (is_mine_p sbs)
      | _        -> 
          (* 4. sinon, (|a| <> 1) et a <> 0 *)
          (* 4.1. on rend le coef a positif s'il ne l'est pas deja *)
          let a, l, b = 
            if compare_num a (Int 0) < 0  then 
	      (minus_num a,
	       List.map (fun (a,x) -> minus_num a,x) l, (minus_num b))
            else (a, l, b)
          in
          (* 4.2. on reduit le systeme *)
          omega_sigma sbs a x l b

  and omega_sigma sbs a x l b =
    
    (* 1. on definie m qui vaut a + 1 *)
    let m = a +/ Int 1 in

    (* 2. on introduit une variable fraiche *)
    let sigma = X.term_embed (fresh_name ()) in
    
    (* 3. l'application de la formule (5.63) nous donne la valeur du pivot x*)
    let mm_sigma = (minus_num m, sigma) in
    let l_mod = map_monomes (fun a -> mod_sym a m) l mm_sigma in

    (* 3.1. Attention au signe de b : 
       on le passe a droite avant de faire mod_sym, d'ou minus_num *)
    let b_mod = minus_num (mod_sym (minus_num b) m) in
    let p = P.create l_mod b_mod Ty.Tint in

    let sbs = (x, p) :: sbs in
    
    (* 4. on substitue x par sa valeur dans l'equation de depart. 
       Voir la formule (5.64) *)
    let p' = P.add (P.mult_const a p) (P.create l b Ty.Tint) in
   
    (* 5. on resoud sur l'equation simplifiee *)
    let sbs2 = solve_int p' in

    (* 6. on normalise sbs par sbs2 *)
    let sbs =  List.map (fun (x, v) -> x, apply_subst sbs2 v) sbs in

    (* 7. on supprime les liaisons inutiles de sbs2 et on merge avec sbs *)
    let sbs2 = List.filter (fun (y, _) -> y <> sigma) sbs2 in
    List.rev_append sbs sbs2

  and solve_int p = 
    if P.is_empty p then raise Not_found;
    let pgcd = P.pgcd_numerators p in
    let ppmc = P.ppmc_denominators p in
    let p = mult_const p (ppmc // pgcd)  in
    let l, b = P.to_list p in
    if not (is_integer_num b) then raise Exception.Unsolvable;
    omega l b

  let is_null p = 
    if snd (P.to_list p) <>/ (Int 0) then raise Exception.Unsolvable; 
    []

  let solve_int p = 
    try solve_int p with Not_found -> is_null p

  let solve_real p =
    try
      let a, x = P.choose p in
      let p = 
	P.mult 
	  (P.create [] ((Int (-1)) // a) (P.type_info p))
	  (P.remove x p) 
      in
      [x, is_mine p]
    with Not_found -> is_null p
      
  let safe_distribution p = 
    let l, c = P.to_list p in
    let ty = P.type_info p in
    List.fold_left
      (fun p (coef, x) -> P.add p (P.create [coef,x] (Int 0) ty)) 
      (P.create [] c ty) l

  let solve_aux r1 r2 =
    let p1 = embed r1 in
    let p2 = embed r2 in
    let ty = P.type_info p2 in
    let p = P.add p1 (P.mult (P.create [] (Int (-1)) ty) p2) in
    let pp = safe_distribution p in
    if ty = Ty.Treal then solve_real pp else solve_int pp

  let solve r1 r2 =
    let sbs = solve_aux r1 r2 in
    List.fast_sort (fun (a,_) (x,y) -> X.compare x a) sbs

  let print = P.print

  let fully_interpreted sb = 
    match sb with
      | Sy.Op (Sy.Plus | Sy.Minus) -> true
      | _ -> false

  let term_extract _ = None

  module Rel = Fm.Make (X) 
    (struct
       include P 
       let poly_of = embed
       let alien_of = is_mine
     end)
    
end
