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

open Format
open Sig
open Exception

let max_split = Num.Int 1000000

let cc_active = ref true

module type S = sig
  type t

  module TimerCC : Timer.S

  val empty : unit -> t
  val assume : cs:bool -> 
    Literal.LT.t -> Explanation.t -> t -> t * Term.Set.t * int
  val query : Literal.LT.t -> t -> answer
  val class_of : t -> Term.t -> Term.t list
end

module Make (X : Sig.X) = struct    

  module TimerCC = Timer.Make(struct end)

  module Ex = Explanation
  module SetA = Use.SA
  module Use = Use.Make(X)
  module Uf = Uf.Make(X)
  module T = Term
  module A = Literal
  module LR = A.Make(struct type t = X.r include X end)
  module SetT = Term.Set
  module S = Symbols

  module SetX = Set.Make(struct type t = X.r let compare = X.compare end)
    
  (* module Uf = Pptarjan.Uf *)

  type env = { 
    use : Use.t;
    uf : Uf.t ;
    relation : X.Rel.t
  }

  type choice_sign =
    | CPos of int (* The explication of this choice *)
    | CNeg (* The choice has been already negated *)

  type t = { 
    gamma : env;
    gamma_finite : env ;
    choices : (X.r A.view * Num.num * choice_sign * Ex.t) list; 
    (** the choice, the size, choice_sign,  the explication set,
        the explication for this choice. *)
  }

  module Print = struct
    
    let begin_case_split () = ()

    let end_case_split () = ()

    let cc r1 r2 = ()

    let make_cst t ctx = ()

    let add_to_use t = ()
	
    let lrepr fmt = List.iter (fprintf fmt "%a " X.print)

    let leaves t lvs = ()
	
    let contra_congruence a ex = ()

    let split_size sz = ()

    let split_backtrack neg_c ex_c = ()

    let split_assume c ex_c = ()

    let split_backjump c dep = ()

    let assume_literal sa = ()

    let congruent a ex = ()

    let query a = ()

  end
    
  let bottom = Hstring.make "@bottom"
  let one, _ = X.make (Term.make (S.name bottom) [] Ty.Tint)

  let concat_leaves uf l = 
    let rec concat_rec acc t = 
      match  X.leaves (fst (Uf.find uf t)) , acc with
	  [] , _ -> one::acc
	| res, [] -> res
	| res , _ -> List.rev_append res acc
    in
    match List.fold_left concat_rec [] l with
	[] -> [one]
      | res -> res

  let are_equal env ex t1 t2 = 
    if T.equal t1 t2 then ex
    else match Uf.are_equal env.uf t1 t2 with
      | Yes dep -> Ex.union ex dep
      | No -> raise Exit

  let equal_only_by_congruence env ex t1 t2 acc = 
    if T.equal t1 t2 then acc
    else
      let {T.f=f1; xs=xs1; ty=ty1} = T.view t1 in
      if X.fully_interpreted f1 then acc
      else 
	let {T.f=f2; xs=xs2; ty=ty2} = T.view t2 in
        if Symbols.equal f1 f2 && Ty.equal ty1 ty2 then
	  try
            let ex = List.fold_left2 (are_equal env) ex xs1 xs2 in
            let a = A.LT.make (A.Eq(t1, t2)) in
            Print.congruent a ex;
            (LTerm a, ex) :: acc
          with Exit -> acc
        else acc

  let congruents env t1 s acc ex = 
    SetT.fold (equal_only_by_congruence env ex t1) s acc

  let fold_find_with_explanation find ex l = 
    List.fold_left 
      (fun (lr, ex) t -> let r, ex_r = find t in r::lr, Ex.union ex_r ex)
      ([], ex) l

  let view find va ex_a = 
    match va with
      | A.Eq (t1, t2) ->
          let r1, ex1 = find t1 in
	  let r2, ex2 = find t2 in
	  let ex = Ex.union (Ex.union ex1 ex2) ex_a in
	  A.Eq(r1, r2), ex
      | A.Distinct (b, lt) -> 
	  let lr, ex = fold_find_with_explanation find ex_a lt in 
	  A.Distinct (b, lr), ex
      | A.Builtin(b, s, l) -> 
	  let lr, ex  = fold_find_with_explanation find ex_a l in
	  A.Builtin(b, s, List.rev lr), ex

  let term_canonical_view env a ex_a =  
    view (Uf.find env.uf) (A.LT.view a) ex_a

  let canonical_view env a ex_a = view (Uf.find_r env.uf) a ex_a

  let new_facts_by_contra_congruence env r bol ex = 
    match X.term_extract r with
      | None -> []
      | Some t1 -> 
	  match T.view t1 with
	    | {T.f=f1 ; xs=[x]} -> 
		List.fold_left 
		  (fun acc t2 ->
		     match T.view t2 with
		       | {T.f=f2 ; xs=[y]} when S.equal f1 f2 ->
			   let a = A.LT.make (A.Distinct (false, [x; y])) in
			   let dist = LTerm a in
			   begin match Uf.are_distinct env.uf t1 t2 with
			     | Yes ex' -> 
				 let ex_r = Ex.union ex ex' in
				 Print.contra_congruence a ex_r;
				 (dist, ex_r) :: acc
			     | No -> assert false
			 end
		       | _ -> acc
		  ) [] (Uf.class_of env.uf bol)
	    | _ -> []

  let contra_congruence  = 
    let vrai,_ = X.make T.vrai in
    let faux, _ = X.make T.faux in
    fun env r ex -> 
      if X.equal (fst (Uf.find_r env.uf r)) vrai then
	  new_facts_by_contra_congruence env r T.faux ex
      else if X.equal (fst (Uf.find_r env.uf r)) faux then
	  new_facts_by_contra_congruence env r T.vrai ex
      else []

  let clean_use = 
    List.fold_left 
      (fun env (a, ex) -> 
	 match a with 
	   | LSem _ -> assert false
	   | LTerm t -> 
	       begin
		 match A.LT.view t with
		   | A.Distinct (_, lt) 
		   | A.Builtin (_, _, lt) ->
		       let lvs = concat_leaves env.uf lt in
		       List.fold_left
			 (fun env rx ->
			    let st, sa = Use.find rx env.use in
			    let sa = SetA.remove (t, ex) sa in
			    { env with use = Use.add rx (st,sa) env.use }
			 ) env lvs
		   | _ -> assert false
	       end) 

  let rec congruence_closure env r1 r2 ex = 
    Print.cc r1 r2;
    let uf, res = Uf.union env.uf r1 r2 ex in
    List.fold_left 
      (fun (env, l) (p, touched, v) ->
	 (* we look for use(p) *)
      	 let p_t, p_a = Use.find p env.use in
	 
	 (* we compute terms and atoms to consider for congruence *)
	 let repr_touched = List.map (fun (_,a,_) -> a) touched in
	 let st_others, sa_others = Use.congr_close_up env.use p repr_touched in
	 
	 (* we update use *)
	 let nuse = Use.up_close_up env.use p v in
	 Use.print nuse;
	 
	 (* we check the congruence of the terms. *)
	 let env =  {env with use=nuse} in
	 let new_eqs = 
	   SetT.fold (fun t l -> congruents env t st_others l ex) p_t l in
       	 let touched_atoms = 
	   List.map (fun (x,y,e)-> (LSem(A.Eq(x, y)), e)) touched 
	 in
	 let touched_atoms = SetA.fold (fun (a, ex) acc ->
	   (LTerm a, ex)::acc) p_a touched_atoms in
	 let touched_atoms = SetA.fold (fun (a, ex) acc ->
	   (LTerm a, ex)::acc) sa_others touched_atoms in
	 env, new_eqs @ touched_atoms 
	   
      ) ({env with uf=uf}, [])  res

  let replay_atom env sa = 
    let relation, result = X.Rel.assume env.relation sa in
    let env = { env with relation = relation } in
    let env = clean_use env result.remove in
    env, result.assume

  let rec add_term env choices t ex =
    (* nothing to do if the term already exists *)
    if Uf.mem env.uf t then env, choices
    else begin
      Print.add_to_use t;
      (* we add t's arguments in env *)
      let {T.f = f; xs = xs} = T.view t in
      let env, choices = 
	List.fold_left (fun (env, ch) t -> add_term env ch t ex)
	  (env, choices) xs 
      in
      (* we update uf and use *)
      let nuf, ctx  = Uf.add env.uf t in 
      Print.make_cst t ctx;
      let rt, _ = Uf.find nuf t in (* XXX : ctx only in terms *)

      if !cc_active then
        let lvs = concat_leaves nuf xs in
        let nuse = Use.up_add env.use t rt lvs in
        
        (* If finitetest is used we add the term to the relation *)
        let rel = X.Rel.add env.relation rt in
        Use.print nuse;

        (* we compute terms to consider for congruence *)
        (* we do this only for non-atomic terms with uninterpreted head-symbol *)
        let st_uset = Use.congr_add nuse lvs in
        
        (* we check the congruence of each term *)
        let env = {uf = nuf; use = nuse; relation = rel} in 
        let ct = congruents env t st_uset [] ex in
        let ct = (List.map (fun lt -> LTerm lt, ex) ctx) @ ct in
        assume_literal env choices ct
      else
        let rel = X.Rel.add env.relation rt in
        let env = {env with uf = nuf; relation = rel} in 
        env, choices
    end
	
  and add env choices a ex =
    match A.LT.view a with
      | A.Eq (t1, t2) -> 
	  let env, choices = add_term env choices t1 ex in
	  add_term env choices t2 ex
      | A.Distinct (_, lt) 
      | A.Builtin (_, _, lt) ->
	  let env, choices = List.fold_left 
	    (fun (env, ch) t-> add_term env ch t ex) (env, choices) lt in
	  let lvs = concat_leaves env.uf lt in (* A verifier *)
	  let env = List.fold_left
	    (fun env rx ->
	      let st, sa = Use.find rx env.use in
	      { env with 
		use = Use.add rx (st,SetA.add (a, ex) sa) env.use }
	    ) env lvs
	  in
	  env, choices

  and semantic_view env choices la = 
    List.fold_left 
      (fun (env, choices, lsa) (a, ex) ->
	 match a with 
	   | LTerm a -> 
	       let env, choices = add env choices a ex in
	       let sa, ex = term_canonical_view env a ex in
	       env, choices, (sa, Some a, ex)::lsa

           (* XXX si on fait canonical_view pour
	      A.Distinct, la theorie des tableaux
              part dans les choux *)
	   | LSem (A.Builtin _  (*| A.Distinct _*) as sa) ->
	       let sa, ex = canonical_view env sa ex in
	       env, choices, (sa, None, ex)::lsa
	   | LSem sa ->
	       env, choices, (sa, None, ex)::lsa)
      (env, choices, []) la

  and assume_literal env choices la =
    if la = [] then env, choices
    else 
      let env, choices, lsa = semantic_view env choices la in
      let env, choices =
        List.fold_left
          (fun (env, choices) (sa, _, ex) ->
            Print.assume_literal sa;
            match sa with
              | A.Eq(r1, r2) ->
                  if !cc_active then
        	    let env, l = congruence_closure env r1 r2 ex in
        	    let env, choices = assume_literal env choices l in
        	    let env, choices =
        	      assume_literal env choices (contra_congruence env r1 ex)
        	    in
        	    assume_literal env choices (contra_congruence env r2 ex)
                  else
                    {env with uf = fst(Uf.union env.uf r1 r2 ex)}, choices
              | A.Distinct (false, lr) ->
        	if Uf.already_distinct env.uf lr then env, choices
        	else
        	  {env with uf = Uf.distinct env.uf lr ex}, choices
              | A.Distinct (true, _) -> assert false
              | A.Builtin _ -> env, choices)
          (env, choices) lsa
      in
      let env, l = replay_atom env lsa in
      assume_literal env (choices@l) l

  let look_for_sat ?(bad_last=No) ch t base_env l =
    let rec aux ch bad_last dl base_env li = 
      match li, bad_last with
      | [], _ -> 
	begin
          match X.Rel.case_split base_env.relation with
	    | [] -> 
		{ t with gamma_finite = base_env; choices = List.rev dl }, ch
	    | l ->
	      let l = 
		List.map
		  (fun (c, ex_c, size) ->
                     let exp = Ex.fresh_exp () in
                     let ex_c_exp = Ex.add_fresh exp ex_c in
                     (* A new explanation in order to track the choice *)
                     (c, size, CPos exp, ex_c_exp)) l in
	      let sz =
		List.fold_left
		  (fun acc (a,s,_,_) ->
		     Num.mult_num acc s) (Num.Int 1) (l@dl) in
              Print.split_size sz;
	      if Num.le_num sz max_split then aux ch No dl base_env l
	      else
		{ t with gamma_finite = base_env; choices = List.rev dl }, ch
	end
      | ((c, size, CNeg, ex_c) as a)::l, _ ->
	  let base_env, ch = assume_literal base_env ch [LSem c, ex_c] in
	  aux ch bad_last (a::dl) base_env l

      (** This optimisation is not correct with the current explanation *)
      (* | [(c, size, CPos exp, ex_c)], Yes dep -> *)
      (*       let neg_c = LR.neg (LR.make c) in *)
      (*       let ex_c = Ex.union ex_c dep in *)
      (*       Print.split_backtrack neg_c ex_c; *)
      (*       aux ch No dl base_env [LR.view neg_c, Num.Int 1, CNeg, ex_c] *)

      | ((c, size, CPos exp, ex_c_exp) as a)::l, _ ->
	  try
            Print.split_assume (LR.make c) ex_c_exp;
	    let base_env, ch = assume_literal base_env ch [LSem c, ex_c_exp] in
	    aux ch bad_last (a::dl) base_env l
	  with Exception.Inconsistent dep ->
            match Ex.remove_fresh exp dep with
              | None ->
                (* The choice doesn't participate to the inconsistency *)
                Print.split_backjump (LR.make c) dep;
                raise (Exception.Inconsistent dep)
              | Some dep ->
                (* The choice participates to the inconsistency *)
                let neg_c = LR.neg (LR.make c) in
	        Print.split_backtrack neg_c dep;
	        aux ch No dl base_env [LR.view neg_c, Num.Int 1, CNeg, dep]
    in
    aux ch bad_last (List.rev t.choices) base_env l

  let try_it f t =
    Print.begin_case_split ();
    let r =
      try 
	if t.choices = [] then look_for_sat [] t t.gamma []
	else
	  try
	    let env, lt = f t.gamma_finite in
	    look_for_sat lt t env []
	  with Exception.Inconsistent dep -> 
	    look_for_sat ~bad_last:(Yes dep)
	      [] { t with choices = []} t.gamma t.choices
      with Exception.Inconsistent d ->
	Print.end_case_split ();
	raise (Exception.Inconsistent d)
    in
    Print.end_case_split (); r

  let extract_from_semvalues =
    List.fold_left
      (fun acc r -> 
	 match X.term_extract r with Some t -> SetT.add t acc | _ -> acc) 
      
  let extract_terms_from_choices = 
    List.fold_left 
      (fun acc (a, _, _, _) -> 
	 match a with
	   | A.Eq(r1, r2) -> extract_from_semvalues acc [r1; r2]
	   | A.Distinct (_, l) -> extract_from_semvalues acc l
	   | _ -> acc) 

  let extract_terms_from_assumed = 
    List.fold_left 
      (fun acc (a, _) -> 
	 match a with
	   | LTerm r -> begin
	       match Literal.LT.view r with 
		 | Literal.Eq (t1, t2) -> 
		     SetT.add t1 (SetT.add t2 acc)
		 | Literal.Distinct (_, l) | Literal.Builtin (_, _, l) -> 
		     List.fold_right SetT.add l acc
	     end
	   | _ -> acc)

  let assume ~cs a ex t = 
    let a = LTerm a in
    let gamma, ch = assume_literal t.gamma [] [a, ex] in
    let t = { t with gamma = gamma } in
    let t, ch = 
      if cs then try_it (fun env -> assume_literal env ch [a, ex] ) t
      else t, ch 
    in
    let choices = extract_terms_from_choices SetT.empty t.choices in
    let all_terms = extract_terms_from_assumed choices ch in
    t, all_terms, 1

  let class_of t term = Uf.class_of t.gamma.uf term

  let add_and_process a t =
    let aux a ex env = 
      let gamma, l = add env [] a ex in assume_literal gamma [] l
    in
    let gamma, _ = aux a Ex.empty t.gamma in
    let t = { t with gamma = gamma } in
    let t, _ =  try_it (aux a Ex.empty) t in
    Use.print t.gamma.use; t    

  let query a t =
    Print.query a;
    try
        match A.LT.view a with
	| A.Eq (t1, t2)  ->
	  let t = add_and_process a t in
	  Uf.are_equal t.gamma.uf t1 t2

	| A.Distinct (false, [t1; t2]) -> 
	  let na = A.LT.neg a in
	  let t = add_and_process na t in (* na ? *)
	  Uf.are_distinct t.gamma.uf t1 t2

	| A.Distinct _ -> 
	  assert false (* devrait etre capture par une analyse statique *)

	| _ -> 
	  let na = A.LT.neg a in
	  let t = add_and_process na t in
	  let env = t.gamma in
	  let rna, ex_rna = term_canonical_view env na Ex.empty in
          X.Rel.query env.relation (rna, Some na, ex_rna)
    with Exception.Inconsistent d -> Yes d

  let empty () = 
    let env = { 
      use = Use.empty ; 
      uf = Uf.empty ; 
      relation = X.Rel.empty ();
    }
    in
    let t = { gamma = env; gamma_finite = env; choices = [] } in
    let t, _, _ = 
      assume ~cs:false 
        (A.LT.make (A.Distinct (false, [T.vrai; T.faux]))) Ex.empty t
    in t

end
