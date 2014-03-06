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

open Num
open Format
open Sig

let ale = Hstring.make "<=" 
let alt = Hstring.make "<"
let is_le n = Hstring.compare n ale = 0
let is_lt n = Hstring.compare n alt = 0

let (-@) l1 l2 = List.rev_append l1 l2

module L = Literal
module Sy = Symbols
  
exception NotConsistent of Literal.LT.Set.t

module type EXTENDED_Polynome = sig
  include Polynome.T
  val poly_of : r -> t
  val alien_of : t -> r
end

module Make 
  (X : Sig.X)
  (P : EXTENDED_Polynome with type r = X.r) = struct

  module MP = Map.Make(P)
  module SP = Set.Make(P)
  module SX = Set.Make(struct type t = X.r include X end)
  module MX = Map.Make(struct type t = X.r include X end)
  
  type r = P.r

  module LR = Literal.Make(struct type t = X.r include X end)

  module Seq = 
    Set.Make
      (struct
         type t = r L.view * L.LT.t option * Explanation.t
         let compare (a, _, _) (b, _, _) = 
	   LR.compare (LR.make a) (LR.make b)
       end)
      
  module Inequation = struct
    type t = { 
      ple0 : P.t; 
      is_le : bool;
      dep : (Literal.LT.t * num * P.t * bool) list;
      expl : Explanation.t
    }
	
    let print fmt ineq = fprintf fmt "%a %s 0" P.print ineq.ple0
      (if ineq.is_le then "<=" else "<")

    let create p1 p2 is_le a expl = 
      let p = P.add p1 (P.mult (P.create [] (Int (-1)) (P.type_info p1)) p2) in
      { ple0 = p; is_le = is_le; dep = [a, Int 1, p, is_le]; expl = expl }

    let choose ineq = snd (P.choose ineq.ple0)

    let find x ineq = P.find x ineq.ple0

    let is_monomial ineq = P.is_monomial ineq.ple0

    let pos_neg mx { ple0 = p } = 
      List.fold_left (fun m (c,x) ->
	let cmp = compare_num c (Int 0) in
	if cmp = 0 then m
	else 
	  let (pos, neg) = try MX.find x m with Not_found -> (0,0) in
	  if cmp > 0 then MX.add x (pos+1, neg) m 
	  else MX.add x (pos, neg+1) m ) mx (fst (P.to_list p))
      
  end

  type t = { 
    inequations : (Literal.LT.t * Inequation.t) list ;
    monomes: (Intervals.t * SX.t) MX.t;
    polynomes : Intervals.t MP.t;
    known_eqs : SX.t;
    improved : SP.t;
  }

  module Debug = struct
 
    let list_of_ineqs fmt = List.iter (fprintf fmt "%a  " Inequation.print)

    let assume a = ()
	
    let cross x cpos cneg others ninqs = ()

    let print_use fmt use = 
      SX.iter (fprintf fmt "%a, " X.print) use

    let env env = ()

    let implied_equalities l = ()
  end
      
  let empty _ = { 
    inequations = [] ; 
    monomes = MX.empty ; 
    polynomes = MP.empty ; 
    known_eqs = SX.empty ; 
    improved = SP.empty ; 
  }

  let replace_inequation env x ineq = 
    { env with
	inequations = (x, ineq)::(List.remove_assoc x env.inequations) }


  let up_improved env p oldi newi =
    if Intervals.is_strict_smaller newi oldi then
      { env with improved = SP.add p env.improved }
    else env
    
(*
  let oldify_inequations env =
    { env with
	inequations = env.inequations@env.new_inequations;
      new_inequations = [] } *)
    
  let mult_bornes_vars vars monomes ty=
    List.fold_left
      (fun ui (y,n) ->
	 let ui' = try
	   fst (MX.find y monomes)
	 with Not_found -> Intervals.undefined ty
	 in
	 Intervals.mult ui (Intervals.power n ui')
      ) (Intervals.point (Int 1) ty Explanation.empty) vars 


  let intervals_from_monomes env p =
    let pl, v = P.to_list p in
    List.fold_left
      (fun i (a, x) ->
	 let i_x, _  = MX.find x env.monomes in
	 Intervals.add (Intervals.scale a i_x) i
      ) (Intervals.point v (P.type_info p) Explanation.empty) pl

  let rec add_monome expl use_x env x =
    try 
      let u, old_use_x = MX.find x env.monomes in
      { env with monomes = MX.add x (u, SX.union old_use_x use_x) env.monomes }
    with Not_found -> 
      update_monome expl use_x env x

  and init_monomes env p use_p expl = 
    List.fold_left
      (fun env (_, x) -> add_monome expl use_p env x)
      env (fst (P.to_list p))

  and init_alien expl p (normal_p, c, d) ty use_x env =
    let env = init_monomes env p use_x expl in
    let i = intervals_from_monomes env p in
    let i = 
      try 
	let old_i = MP.find normal_p env.polynomes in
	let old_i = Intervals.scale d
	  (Intervals.add old_i (Intervals.point c ty Explanation.empty)) in
	Intervals.intersect i old_i
      with Not_found -> i
    in
    env, i

      

  and update_monome expl use_x env x =
    let ty = X.type_info x in
    let ui, env = 
      match X.term_extract x with
	| Some t ->
	    let use_x = SX.singleton x in
	    begin
	      match Term.view t with
		| {Term.f = (Sy.Op Sy.Div); xs = [a; b]} ->
		  let pa = P.poly_of (fst (X.make a)) in
		  let pb = P.poly_of (fst (X.make b)) in
		  let (pa', ca, da) as npa = P.normal_form_pos pa in
		  let (pb', cb, db) as npb = P.normal_form_pos pb in
		  let env, ia = init_alien expl pa npa ty use_x env in
		  let env, ib = init_alien expl pb npb ty use_x env in
		  let ia, ib = match Intervals.doesnt_contain_0 ib with
		    | Yes ex when Num.compare_num ca cb = 0 
			       && P.compare pa' pb' = 0 ->
		      let expl = Explanation.union ex expl in
		      Intervals.point da ty expl, Intervals.point db ty expl
		    | _ -> ia, ib
		  in
		  Intervals.div ia ib, env
		| _ -> Intervals.undefined ty, env
	    end
	| _ -> Intervals.undefined ty, env
    in
    let u, use_x' =
      try MX.find x env.monomes
      with Not_found -> Intervals.undefined (X.type_info x), use_x in
    let ui = Intervals.intersect ui u in
    { env with monomes = MX.add x (ui, (SX.union use_x use_x')) env.monomes }
      
  and tighten_div x env expl = env

  and tighten_non_lin x use_x env expl =
    let env = tighten_div x env expl in
    SX.fold 
      (fun x acc -> 
	let _, use = MX.find x acc.monomes in
	  update_monome expl use acc x)
      use_x env

  let update_monomes_from_poly p i polynomes monomes =
    let lp, _ = P.to_list p in
    let ty = P.type_info p in
    List.fold_left (fun monomes (a,x) ->
      let np = P.remove x p in
      let (np,c,d) = P.normal_form_pos np in
      try 
	let inp = MP.find np polynomes in
	let new_ix =
	  Intervals.scale 
	    ((Int 1) // a)
	    (Intervals.add i
	       (Intervals.scale (minus_num d)
		  (Intervals.add inp 
		     (Intervals.point c ty Explanation.empty)))) in
	let old_ix, ux = MX.find x monomes in
	let ix = Intervals.intersect old_ix new_ix in
	MX.add x (ix, ux) monomes
      with Not_found -> monomes)
      monomes lp

  let update_polynomes env expl =
    let polynomes, monomes, improved = MP.fold
      (fun p ip (polynomes, monomes, improved) ->
	 let new_i = intervals_from_monomes env p in
	 let i = Intervals.intersect new_i ip in
	 if Intervals.is_strict_smaller i ip then
	   let monomes = update_monomes_from_poly p i polynomes monomes in
	   let improved = SP.add p improved in
	   MP.add p i polynomes, monomes, improved
	 else polynomes, monomes, improved
      ) env.polynomes (env.polynomes, env.monomes, env.improved) in
    {env with polynomes = polynomes; monomes = monomes ; improved = improved}


  let find_one_eq x u =
    match Intervals.is_point u with
      | Some (v, ex) when X.type_info x <> Ty.Tint or is_integer_num v ->
          let eq = 
	    L.Eq (x,(P.alien_of (P.create [] v (X.type_info x)))) in
	  Some (eq, None, ex)
      | _ -> None

  let find_eq eqs x u env =
    match find_one_eq x u with
      | None -> eqs
      | Some eq1 -> eq1::eqs

  type ineq_status = 
    | Trivial_eq
    | Trivial_ineq of num
    | Bottom
    | Monome of num * P.r * num
    | Other

  let ineq_status ({Inequation.ple0 = p ; is_le = is_le} as ineq) = 
    match Inequation.is_monomial ineq with
	Some (a, x, v) -> Monome (a, x, v)
      | None -> 
	  if P.is_empty p then
	    let _, v = P.to_list p in 
	    let c = compare_num v (Int 0) in
	    if c > 0 || (c >=0 && not is_le) then Bottom
	    else 
	      if c = 0 && is_le then Trivial_eq
	      else Trivial_ineq v
	  else Other
	    
  (*let ineqs_from_dep dep borne_inf is_le =
    List.map
      (fun {poly_orig = p; coef = c} -> 
	 let (m,v,ty) = P.mult_const minusone p in
	 (* quelle valeur pour le ?????? *)
	 { ple0 = {poly = (m, v +/ (borne_inf // c), ty); le = is_le} ;
	   dep = []}
      )dep*)

  let mk_equality p =
    let r1 = P.alien_of p in
    let r2 = P.alien_of (P.create [] (Int 0) (P.type_info p)) in
    L.Eq (r1, r2)

  let fm_equalities env eqs { Inequation.ple0 = p; dep = dep; expl = ex } =
    let inqs, eqs =
      List.fold_left
	(fun (inqs, eqs) (a, _, p, _) -> 
           List.remove_assoc a inqs, (mk_equality p, Some a, ex) :: eqs
	) (env.inequations, eqs) dep
    in
    { env with inequations = inqs }, eqs

  let update_intervals env eqs expl (a, x, v) is_le =
    let uints, use_x = MX.find x env.monomes in
    let b = ((Int (-1)) */ v) // a in
    let u =
      if a >/ (Int 0) then
	Intervals.new_borne_sup expl b is_le uints
      else   
	Intervals.new_borne_inf expl b is_le uints in
    let env = { env with monomes = MX.add x (u, use_x) env.monomes } in
    let env =  tighten_non_lin x use_x env expl in
    env, (find_eq eqs x u env)
  
  let update_ple0 env p0 is_le expl =
    if P.is_empty p0 then env
    else 
      let ty = P.type_info p0 in
      let a, _ = P.choose p0 in
      let p, change =
	if a </ Int 0 then
	  P.mult (P.create [] (Int (-1)) ty) p0, true
	else p0, false in
      let p, c, _ = P.normal_form p in
      let c = minus_num c in
      let u =
	if change then
          Intervals.new_borne_inf expl c is_le (Intervals.undefined ty)
	else
	  Intervals.new_borne_sup expl c is_le (Intervals.undefined ty) in
      let u, pu =
	try 
	  let pu = MP.find p env.polynomes in
	  let i = Intervals.intersect u pu in
	  i, pu
	with Not_found -> u, Intervals.undefined ty
      in
      let env = 
	if Intervals.is_strict_smaller u pu then
	  let polynomes = MP.add p u env.polynomes in
	  let monomes = update_monomes_from_poly p u polynomes env.monomes in
	  let improved = SP.add p env.improved in
	  { env with 
	    polynomes = polynomes; 
	    monomes = monomes; 
	    improved = improved }
	else env
      in
      match P.to_list p0 with
        | [a,x], v -> fst(update_intervals env [] expl (a, x, v) is_le)
        | _ -> env

  let add_inequations acc lin expl = 
    List.fold_left
      (fun (env, eqs) ineq ->
	(* let expl = List.fold_left 
	  (fun expl (l,_,_,_) -> 
	    Explanation.union (*Explanation.everything*)
	      (Explanation.singleton (Formula.mk_lit l))
	      expl
	  ) expl ineq.Inequation.dep 
	in *)
	let expl = Explanation.union ineq.Inequation.expl expl in
	 match ineq_status ineq with
	   | Bottom           ->
	       raise (Exception.Inconsistent expl)
		 
	   | Trivial_eq       -> 
	       fm_equalities env eqs ineq
		 
	   | Trivial_ineq  c  ->
	       let n, pp = 
		 List.fold_left 
		   (fun ((n, pp) as acc) (_, _, p, is_le) ->  
		      if is_le then acc else 
			match pp with
			  | Some _ -> n+1, None
			  | None when n=0 -> 1, Some p
			  | _ -> n+1, None) (0,None) ineq.Inequation.dep
		    in
	       let env = 
		 List.fold_left
		   (fun env (_, coef, p, is_le) ->
		      let ty = P.type_info p in
		      let is_le = 
			match pp with 
			    Some x -> P.compare x p = 0 | _ -> is_le && n=0 
		      in
		      let p' = P.sub (P.create [] (c // coef) ty) p in
		      update_ple0 env p' is_le expl
		   ) env ineq.Inequation.dep
	       in
	       env, eqs

	   | Monome (a, x, v) ->
	       let env, eqs = 
		 update_intervals env eqs expl (a, x, v) ineq.Inequation.is_le
	       in
               
	       (*let env,eqs = update_bornes env eqs ((a,x),c) ineq.ple0.le in
		 let env,eqs = update_polynomes env eqs ineq in
		 env, pers_ineqs, eqs*)
	       env, eqs

	   | Other            -> 
	       env, eqs
	       (*t env,eqs = update_polynomes env eqs ineq in
	       env, pers_ineqs, eqs*)

	       
      ) acc lin

  let mult_list c = 
    List.map (fun (a, coef, p, is_le) -> (a, coef */ c, p, is_le))

  let div_by_pgcd (a, b) ty =
    try
      if ty = Ty.Tint then
	let p = Big_int.gcd_big_int (big_int_of_num a) (big_int_of_num b) in
	let p = num_of_big_int p in
	a // p, b // p
      else a, b
    with Failure "big_int_of_ratio" -> a, b

  let cross x cpos cneg = 
    let rec cross_rec acc = function 
      | [] -> acc
      | { Inequation.ple0 = p1; is_le = k1; dep = d1; expl = ex1 } :: l ->
	  let n1 = abs_num (P.find x p1) in
	  (* let ty = P.type_info p1 in *)
	  let acc = 
	    List.fold_left 
	      (fun acc {Inequation.ple0 = p2; is_le = k2; dep=d2; expl = ex2} ->
		 let n2 = abs_num (P.find x p2) in
		 (* let n1, n2 =  div_by_pgcd (n1, n2) ty in *)
		 let p = P.add
		   (P.mult (P.create [] n2 (P.type_info p2)) p1)
		   (P.mult (P.create [] n1 (P.type_info p1)) p2) in
		 let d1 = mult_list n2 d1 in
		 let d2 = mult_list n1 d2 in
		 let ni = 
		   { Inequation.ple0 = p;  is_le = k1&&k2; dep = d1 -@ d2;
		     expl = Explanation.union ex1 ex2 }
		 in 
		 ni::acc
	      ) acc cpos
	  in 
	  cross_rec acc l
    in
    cross_rec [] cneg

  let split x l = 
    let rec split_rec (cp, cn, co) ineq =
      try
	let a = Inequation.find x ineq in
	if a >/ (Int 0) then ineq::cp, cn, co 
	else cp, ineq::cn, co
      with Not_found ->	cp, cn, ineq::co
    in 
    List.fold_left split_rec ([], [], []) l

  let length s = SX.fold (fun _ acc -> acc+1) s 0          

  let choose_var l = 
    let pos_neg = List.fold_left Inequation.pos_neg MX.empty l in
    let xopt = MX.fold (fun x (pos, neg) acc ->
      match acc with
	| None -> Some (x, pos * neg)
	| Some (y, c') -> 
	  let c = pos * neg in 
	  if c < c' then Some (x, c) else acc
    ) pos_neg None in
    match xopt with
      | Some (x, _) -> x
      | None -> raise Not_found

  let rec fourier ( (env, eqs) as acc) l expl =
     match l with
      | [] -> acc
      | ineq :: l' ->
	try
	  (* let x = Inequation.choose ineq in *)
	  let x = choose_var l in
	  let cpos, cneg, others = split x l in
	  let ninqs = cross x cpos cneg in
	  Debug.cross x cpos cneg others ninqs;
	  let acc = add_inequations acc cpos expl in
	  let acc = add_inequations acc cneg expl in
	  fourier acc (ninqs -@ others) expl
	with Not_found -> add_inequations acc l expl

  (*
  let fm env eqs expl = 
    fourier (env, eqs)
      (List.map snd env.inequations)
      (List.map snd env.new_inequations) expl
*)

  let fm env eqs expl = 
    fourier (env, eqs) (List.map snd env.inequations) expl

  let is_num r = 
    let ty = X.type_info r in ty = Ty.Tint || ty = Ty.Treal

  let add_disequality env eqs p expl =
    let ty = P.type_info p in
    match P.to_list p with
      | ([], (Int 0)) ->
	  raise (Exception.Inconsistent expl)
      | ([], v) ->
	  env, eqs
      | ([a, x], v) -> 
	  let b = (minus_num v) // a in
	  let i1 = Intervals.point b ty expl in
	  let i2, use2 = 
	    try 
	      MX.find x env.monomes 
	    with Not_found -> Intervals.undefined ty, SX.empty
	  in
	  let i = Intervals.exclude i1 i2 in
	  let env ={ env with monomes = MX.add x (i,use2) env.monomes } in
	  let env = tighten_non_lin x use2 env expl in
	  env, find_eq eqs x i env
      | _ ->
	  let a, _ = P.choose p in
	  let p = if a >=/ Int 0 then p
	  else P.mult (P.create [] (Int (-1)) ty) p in
	  let p, c, _ = P.normal_form p in
	  let i1 = Intervals.point (minus_num c) ty expl in
	  let i2 = 
	    try 
	      MP.find p env.polynomes 
	    with Not_found -> Intervals.undefined ty
	  in
	  let i = Intervals.exclude i1 i2 in
	  let env = 
	    if Intervals.is_strict_smaller i i2 then
	      let polynomes = MP.add p i env.polynomes in
	      let monomes = update_monomes_from_poly p i polynomes env.monomes
	      in
	      let improved = SP.add p env.improved in
	      { env with 
		polynomes = polynomes;
		monomes = monomes;
		improved = improved}
	    else env
	  in
	  env, eqs
					      
  let add_equality env eqs p expl =
    let ty = P.type_info p in
    match P.to_list p with	
      | ([], Int 0) -> env, eqs
      | ([], v) ->
	  raise (Exception.Inconsistent expl)
      | ([a, x], v) -> 
	  let b = (minus_num v) // a in
	  let i = Intervals.point b ty expl in
	  let i, use = 
	    try 
	      let i', use' = MX.find x env.monomes in
	      Intervals.intersect i i', use'
	    with Not_found -> i, SX.empty
	  in
	  let env = { env with monomes = MX.add x (i, use) env.monomes} in
	  let env = tighten_non_lin x use env expl in
	  env, find_eq eqs x i env
      | _ ->
	  let a, _ = P.choose p in
	  let p = if a >=/ Int 0 then p
	  else P.mult (P.create [] (Int (-1)) ty) p in
	  let p, c, _ = P.normal_form p in
	  let i = Intervals.point (minus_num c) ty expl in
	  let i, ip = 
	    try
	      let ip =  MP.find p env.polynomes in
	      Intervals.intersect i ip, ip
	    with Not_found -> i, Intervals.undefined ty
	  in
	  let env = 
	    if Intervals.is_strict_smaller i ip then
	      let polynomes = MP.add p i env.polynomes in
	      let monomes = update_monomes_from_poly p i polynomes env.monomes
	      in
	      let improved = SP.add p env.improved in
	      { env with 
		polynomes = polynomes;
		monomes = monomes;
		improved = improved }
	    else env
	  in
	  let env = 
	    { env with 
	      known_eqs = SX.add (P.alien_of p) env.known_eqs
            } in
	  env, eqs

  let normal_form a = match a with
    | L.Builtin (false, n, [r1; r2]) when is_le n && X.type_info r1 = Ty.Tint ->
        let pred_r1 = P.sub (P.poly_of r1) (P.create [] (Int 1) Ty.Tint) in
	L.Builtin (true, n, [r2; P.alien_of pred_r1])

    | L.Builtin (true, n, [r1; r2]) when 
	not (is_le n) && X.type_info r1 = Ty.Tint ->
        let pred_r2 = P.sub (P.poly_of r2) (P.create [] (Int 1) Ty.Tint) in
	L.Builtin (true, ale, [r1; P.alien_of pred_r2])

    | L.Builtin (false, n, [r1; r2]) when is_le n -> 
	L.Builtin (true, alt, [r2; r1])

    | L.Builtin (false, n, [r1; r2]) when is_lt n ->
	L.Builtin (true, ale, [r2; r1])

    | _ -> a
	  
  let remove_trivial_eqs eqs la =
      let set_of l =
        List.fold_left (fun s e -> Seq.add e s) Seq.empty l
      in
      Seq.elements (Seq.diff (set_of eqs) (set_of la))
          

  let equalities_from_polynomes env eqs =
    let known, eqs = 
      MP.fold
      (fun p i (knw, eqs) ->
        let xp = P.alien_of p in
         if SX.mem xp knw then knw, eqs
         else 
           match Intervals.is_point i with
             | Some (num, ex) ->
               let r2 = P.alien_of (P.create [] num (P.type_info p)) in
               SX.add xp knw, (L.Eq(xp, r2), None, ex) :: eqs
           | None -> knw, eqs
      ) env.polynomes  (env.known_eqs, eqs)
    in {env with known_eqs= known}, eqs



  let equalities_from_monomes env eqs =
    let known, eqs = 
      MX.fold
        (fun x (i,_) (knw, eqs) ->
          if SX.mem x knw then knw, eqs
          else 
            match Intervals.is_point i with
              | Some (num, ex) ->
                let r2 = P.alien_of (P.create [] num (X.type_info x)) in
                SX.add x knw, (L.Eq(x, r2), None, ex) :: eqs
              | None -> knw, eqs
        ) env.monomes  (env.known_eqs, eqs)
    in {env with known_eqs= known}, eqs

  let equalities_from_intervals env eqs =
    let env, eqs = equalities_from_polynomes env eqs in
    equalities_from_monomes env eqs

  let assume env la =
    let env = {env with improved = SP.empty} in
    Debug.env env;
    let env, eqs, new_ineqs, expl =
      List.fold_left
	(fun (env, eqs, new_ineqs, expl) (a, root, e) ->
	   let a = normal_form a in
	   let expl = Explanation.union e expl in
	   try
             match a with
	       | L.Builtin(_, n, [r1;r2]) when is_le n || is_lt n ->
                   let root = match root with
	             | Some a -> a | None -> assert false in
		   let p1 = P.poly_of r1 in
		   let p2 = P.poly_of r2 in
		   let ineq = Inequation.create p1 p2 (is_le n) root expl in
		   let env =
		     init_monomes env ineq.Inequation.ple0 SX.empty expl in
		   let env =
		     update_ple0 env ineq.Inequation.ple0 (is_le n) expl in
		   let env = replace_inequation env root ineq in
		   env, eqs, true, expl

	       | L.Distinct (false, [r1; r2]) when is_num r1 && is_num r2 -> 
		   let p = P.sub (P.poly_of r1) (P.poly_of r2) in
		   let env = init_monomes env p SX.empty expl in
		   let env, eqs = add_disequality env eqs p expl in
                   env, eqs, new_ineqs, expl
		     
	       | L.Eq(r1, r2) when is_num r1 && is_num r2 -> 
		   let p = P.sub (P.poly_of r1) (P.poly_of r2) in
		   let env = init_monomes env p SX.empty expl in
		   let env, eqs = add_equality env eqs p expl in
                   env, eqs, new_ineqs, expl

	       | _ -> (env, eqs, new_ineqs, expl) 
		   
	   with Intervals.NotConsistent expl ->
	     raise (Exception.Inconsistent expl)
	)
	(env, [], false, Explanation.empty) la 
	
    in
    if new_ineqs then 
      if false then 
	(); 
    try
      (* we only call fm when new ineqs are assumed *)
      let env, eqs = if new_ineqs then fm env eqs expl else env, eqs in
      (* let env = oldify_inequations env in *)
      let env = update_polynomes env expl in
      let env, eqs = equalities_from_intervals env eqs in
      Debug.env env;
      let eqs = remove_trivial_eqs eqs la in
      Debug.implied_equalities eqs;
      let result = 
	List.fold_left 
	  (fun r (a_sem, a_term, ex) -> 
	     { assume = (LSem(a_sem), ex) :: r.assume; 
	       remove = 
		 match a_term with 
		   | None -> r.remove 
		   | Some t -> (LTerm(t), ex)::r.remove
	     } ) { assume = []; remove = [] } eqs
      in
      env, result

    with Intervals.NotConsistent expl ->
      raise (Exception.Inconsistent expl)
      
  let query env a_ex =
    try 
      ignore(assume env [a_ex]); 
      No
    with Exception.Inconsistent expl -> Yes expl

  let case_split_polynomes env = 
    let o = MP.fold
      (fun p i o ->
	 match Intervals.finite_size i with
	   | Some s when s >/ (Int 1) ->
	       begin
		 match o with
		   | Some (s', _, _, _) when s' <=/ s -> o
		   | _ -> 
		       let n, ex = Intervals.borne_inf i in
		       Some (s, p, n, ex)
	       end
	   | _ -> o
      ) env.polynomes None in
    match o with 
      | Some (s, p, n, ex) -> 
          let r1 = P.alien_of p in
	  let r2 = P.alien_of (P.create [] n  (P.type_info p)) in
	  [L.Eq(r1, r2), ex, s]
      | None -> 
	  []

  let case_split_monomes env = 
    let o = MX.fold
      (fun x (i,_) o ->
	 match Intervals.finite_size i with
	   | Some s when s >/ (Int 1) ->
	       begin
		 match o with
		   | Some (s', _, _, _) when s' <=/ s -> o
		   | _ -> 
		       let n, ex = Intervals.borne_inf i in
		       Some (s, x, n, ex)
	       end
	   | _ -> o
      ) env.monomes None in
    match o with 
      | Some (s,x,n,ex) -> 
          let ty = X.type_info x in
          let r1 = x in
	  let r2 = P.alien_of (P.create [] n  ty) in
	  [L.Eq(r1, r2), ex, s]
      | None -> 
	  []
   
  let case_split env = 
    match case_split_polynomes env with
      | []     -> case_split_monomes env
      | choices -> choices
   
  let add env _ = env

  let extract_improved env =
    SP.fold
      (fun p acc ->
	MP.add p (MP.find p env.polynomes) acc)
      env.improved MP.empty

end
