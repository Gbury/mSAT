(**************************************************************************)
(*                                                                        *)
(*                          Alt-Ergo Zero                                 *)
(*                                                                        *)
(*                  Sylvain Conchon and Alain Mebsout                     *)
(*                      Universite Paris-Sud 11                           *)
(*                                                                        *)
(*  Copyright 2011. This file is distributed under the terms of the       *)
(*  Apache Software License version 2.0                                   *)
(*                                                                        *)
(**************************************************************************)

open Format

type error = 
  | DuplicateTypeName of Hstring.t
  | DuplicateSymb of Hstring.t
  | UnknownType of Hstring.t
  | UnknownSymb of Hstring.t

exception Error of error

module AETerm = Term
module H = Hstring.H
module HSet = Hstring.HSet

let decl_types = H.create 17
let decl_symbs = H.create 17

let htrue = Hstring.make "True"
let hfalse = Hstring.make "False"

module Type = struct

  type t = Hstring.t

  let equal = Hstring.equal

  let type_int = 
    let tint = Hstring.make "int" in
    H.add decl_types tint Ty.Tint;
    tint

  let type_real = 
    let treal = Hstring.make "real" in
    H.add decl_types treal Ty.Treal;
    treal

  let type_bool = 
    let tbool = Hstring.make "bool" in
    H.add decl_types tbool Ty.Tbool;
    tbool

  let type_proc = 
    let tproc = Hstring.make "proc" in
    H.add decl_types tproc Ty.Tint;
    tproc

  let declare_constructor ty c = 
    if H.mem decl_symbs c then raise (Error (DuplicateSymb c));
    H.add decl_symbs c 
      (Symbols.name ~kind:Symbols.Constructor c, [], ty)

  let declare t constrs = 
    if H.mem decl_types t then raise (Error (DuplicateTypeName t));
    match constrs with
      | [] -> 
	  H.add decl_types t (Ty.Tabstract t)
      | _ -> 
	  let ty = Ty.Tsum (t, constrs) in
	  H.add decl_types t ty;
	  List.iter (fun c -> declare_constructor t c) constrs

  let all_constructors () =
    H.fold (fun _ c acc -> match c with
      | Symbols.Name (h, Symbols.Constructor), _, _ -> h :: acc
      | _ -> acc
    ) decl_symbs [htrue; hfalse]

  let constructors ty =
    if Hstring.equal ty type_bool then [htrue; hfalse]
    else match H.find decl_types ty with
      | Ty.Tsum (_ , cstrs) -> cstrs
      | _ -> raise Not_found
    
end

module Symbol = struct
    
  type t = Hstring.t

  let declare f args ret  = 
    if H.mem decl_symbs f then raise (Error (DuplicateTypeName f));
    List.iter 
      (fun t -> 
	if not (H.mem decl_types t) then raise (Error (UnknownType t)) )
      (ret::args);
    H.add decl_symbs f (Symbols.name f, args, ret)

  let type_of s = let _, args, ret = H.find decl_symbs s in args, ret

  let declared s = 
    let res = H.mem decl_symbs s in
    if not res then begin 
      eprintf "Not declared : %a in@." Hstring.print s;
      H.iter (fun hs (sy, _, _) ->
	eprintf "%a (=?%b) -> %a@." Hstring.print hs 
	  (Hstring.compare hs s = 0)
	  Symbols.print sy)
	  decl_symbs;
      end;
      res

  let not_builtin ty = Hstring.equal ty Type.type_proc ||
    not (Hstring.equal ty Type.type_int || Hstring.equal ty Type.type_real ||
	   Hstring.equal ty Type.type_bool || Hstring.equal ty Type.type_proc)
    
  let has_abstract_type s =
    let _, ret = type_of s in
    match H.find decl_types ret with
      | Ty.Tabstract _ -> true
      | _ -> false
     
  let has_type_proc s =
    Hstring.equal (snd (type_of s)) Type.type_proc
      
  let _ = 
    H.add decl_symbs htrue (Symbols.True, [], Type.type_bool);
    H.add decl_symbs hfalse (Symbols.False, [], Type.type_bool);
    
end


module Variant = struct
    
  let constructors = H.create 17
  let assignments = H.create 17

  let find t x = try H.find t x with Not_found -> HSet.empty
    
  let add t x v = 
    let s = find t x in
    H.replace t x (HSet.add v s)
      
  let assign_constr = add constructors
    
  let assign_var x y = 
    if not (Hstring.equal x y) then
      add assignments x y
	
  let rec compute () = 
    let flag = ref false in
    let visited = ref HSet.empty in
    let rec dfs x s = 
      if not (HSet.mem x !visited) then
	begin
	  visited := HSet.add x !visited;
	  HSet.iter 
	    (fun y -> 
	      let c_x = find constructors x in
	      let c_y = find constructors y in
	      let c = HSet.union c_x c_y in
	      if not (HSet.equal c c_x) then
		begin
		     H.replace constructors x c;
		     flag := true
		end;
	      dfs y (find assignments y)
	    ) s
	end
    in
    H.iter dfs assignments;
    if !flag then compute ()
      
  let hset_print fmt s = 
    HSet.iter (fun c -> Format.eprintf "%a, " Hstring.print c) s
      
  let print () = 
      H.iter 
	(fun x c -> 
	  Format.eprintf "%a = {%a}@." Hstring.print x hset_print c) 
	constructors
	
	
  let get_variants = H.find constructors
    
  let set_of_list = List.fold_left (fun s x -> HSet.add x s) HSet.empty 
    
  let init l = 
    compute ();
    List.iter 
      (fun (x, nty) -> 
	if not (H.mem constructors x) then
	  let ty = H.find decl_types nty in
	  match ty with
	    | Ty.Tsum (_, l) ->
	      H.add constructors x (set_of_list l)
	    | _ -> ()) l;
    H.clear assignments

  let update_decl_types s = 
    let nty = ref "" in
    let l = ref [] in
    HSet.iter 
      (fun x -> 
	l := x :: !l; 
	let vx = Hstring.view x in 
	nty := if !nty = "" then vx else !nty ^ "|" ^ vx) s;
    let nty = Hstring.make !nty in
    let ty = Ty.Tsum (nty, List.rev !l) in
    H.replace decl_types nty ty;
    nty

  let close () = 
    compute ();
    H.iter 
      (fun x s -> 
	let nty = update_decl_types s in
	let sy, args, _ = H.find decl_symbs x in
	H.replace decl_symbs x (sy, args, nty))
      constructors
      
end
  

module rec Term : sig

  type t = T of AETerm.t | Tite of Formula.t * t * t

  type operator = Plus | Minus | Mult | Div | Modulo

  val first_ite : t list -> t list * (Formula.t * t * t) * t list
  val make_int : Num.num -> t
  val make_real : Num.num -> t
  val make_app : Symbol.t -> t list -> t
  val make_arith : operator -> t -> t -> t
  val make_ite : Formula.t -> t -> t -> t
  val is_int : t -> bool
  val is_real : t -> bool
  val t_true : t
  val t_false : t

end
= struct

  type t = T of AETerm.t | Tite of Formula.t * t * t
  type operator = Plus | Minus | Mult | Div | Modulo

  let make_int i = T (AETerm.int (Num.string_of_num i))

  let make_real r = T (AETerm.real (Num.string_of_num r))

  let rec first_ite = function
    | [] -> raise Not_found
    | Tite (c, t1, t2) :: l -> [], (c, t1, t2), l
    | x :: l -> 
      let left, triplet, right = first_ite l in
      x::left, triplet, right

  let rec lift_ite sb l ty = 
    try
      let left, (c, t1, t2), right = first_ite l in
      let l = lift_ite sb (left@(t1::right)) ty in
      let r = lift_ite sb (left@(t2::right)) ty in
      Tite (c, l, r)
    with Not_found -> 
      let l = List.map (function T x -> x | _ -> assert false) l in
      T (AETerm.make sb l ty)

  let make_app s l =
    try
      let (sb, _, nty) = H.find decl_symbs s in
      let ty = H.find decl_types nty in
      lift_ite sb l ty
    with Not_found -> raise (Error (UnknownSymb s))

  let t_true = T AETerm.vrai
  let t_false = T AETerm.faux

  let rec is_int = function
    | T t -> AETerm.is_int t
    | Tite(_, t1, t2) -> is_int t1 && is_int t2

  let rec is_real = function
    | T t -> AETerm.is_real t
    | Tite(_, t1, t2) -> is_real t1 && is_real t2

  let make_arith op t1 t2 = 
    let op = 
      match op with
	| Plus -> Symbols.Plus
	| Minus -> Symbols.Minus
	| Mult ->  Symbols.Mult
	| Div -> Symbols.Div
	| Modulo -> Symbols.Modulo
    in
    let ty = 
      if is_int t1 && is_int t2 then Ty.Tint
      else if is_real t1 && is_real t2 then Ty.Treal
      else assert false
    in
    lift_ite (Symbols.Op op) [t1; t2] ty

  let make_ite l t1 t2 = Tite (l, t1, t2)


end

and Formula : sig

  type comparator = Eq | Neq | Le | Lt  
  type combinator = And | Or | Imp | Not
  type t = 
    | Lit of Literal.LT.t  
    | Comb of combinator * t list

  val f_true : t
  val f_false : t
  val make_lit : comparator -> Term.t list -> t
  val make : combinator -> t list -> t
  val make_cnf : t -> Literal.LT.t list list

  val print_list : string -> Format.formatter -> t list -> unit
  val print : Format.formatter -> t -> unit

  module Tseitin (Dymmy : sig end) :  
  sig val make_cnf : t -> Literal.LT.t list list end

end
= struct

  type comparator = Eq | Neq | Le | Lt
  type combinator = And | Or | Imp | Not

  type t = 
    | Lit of Literal.LT.t  
    | Comb of combinator * t list

  let rec print fmt phi = 
    match phi with
      | Lit a -> Literal.LT.print fmt a
      | Comb (Not, [f]) -> 
	  fprintf fmt "not (%a)" print f
      | Comb (And, l) -> fprintf fmt "(%a)" (print_list "and") l
      | Comb (Or, l) ->  fprintf fmt "(%a)" (print_list "or") l
      | Comb (Imp, [f1; f2]) -> 
	  fprintf fmt "(%a => %a)" print f1 print f2
      | _ -> assert false
  and print_list sep fmt = function
    | [] -> ()
    | [f] -> print fmt f
    | f::l -> fprintf fmt "%a %s %a" print f sep (print_list sep) l

  let f_true = Lit Literal.LT.vrai
  let f_false = Lit Literal.LT.faux

  let make comb l = Comb (comb, l)

  let value env c = 
    if List.mem c env then Some true
    else if List.mem (make Not [c]) env then Some false
    else None

  let rec lift_ite env op l = 
    try
      let left, (c, t1, t2), right = Term.first_ite l in
      begin
	match value env c with
	  | Some true -> 
	    lift_ite (c::env) op (left@(t1::right))
	  | Some false -> 
	    lift_ite ((make Not [c])::env) op (left@(t2::right))
	  | None ->
	    Comb 
	      (And, 
	       [Comb 
		   (Imp, [c; lift_ite (c::env) op (left@(t1::right))]);
		Comb (Imp, 
		      [(make Not [c]); 
		       lift_ite 
			 ((make Not [c])::env) op (left@(t2::right))])])
      end
    with Not_found -> 
      begin
	let lit =
	  match op, l with
	    | Eq, [Term.T t1; Term.T t2] -> 
	      Literal.Eq (t1, t2)
	    | Neq, ts -> 
	      let ts = 
		List.map (function Term.T x -> x | _ -> assert false) ts in
	      Literal.Distinct (false, ts)
	    | Le, [Term.T t1; Term.T t2] ->
	      Literal.Builtin (true, Hstring.make "<=", [t1; t2])
	    | Lt, [Term.T t1; Term.T t2] ->
	      Literal.Builtin (true, Hstring.make "<", [t1; t2])
	    | _ -> assert false
	in
	Lit (Literal.LT.make lit)
      end

  let make_lit op l = lift_ite [] op l

  let rec sform = function
    | Comb (Not, [Lit a]) -> Lit (Literal.LT.neg a)
    | Comb (Not, [Comb (Not, [f])]) -> sform f
    | Comb (Not, [Comb (Or, l)]) ->
	let nl = List.map (fun a -> sform (Comb (Not, [a]))) l in
	Comb (And, nl)
    | Comb (Not, [Comb (And, l)]) ->  
	let nl = List.map (fun a -> sform (Comb (Not, [a]))) l in
	Comb (Or, nl)
    | Comb (Not, [Comb (Imp, [f1; f2])]) -> 
	Comb (And, [sform f1; sform (Comb (Not, [f2]))])
    | Comb (And, l) -> 
	Comb (And, List.map sform l)
    | Comb (Or, l) -> 
	Comb (Or, List.map sform l)
    | Comb (Imp, [f1; f2]) -> 
	Comb (Or, [sform (Comb (Not, [f1])); sform f2])
    | Comb (Imp, _) -> assert false
    | f -> f

  let make_or = function
    | [] -> assert false
    | [a] -> a
    | l -> Comb (Or, l)

  let distrib l_and l_or = 
    let l = 
      if l_or = [] then l_and
      else
	List.map 
	  (fun x -> 
	     match x with 
	       | Lit _ -> Comb (Or, x::l_or)
	       | Comb (Or, l) -> Comb (Or, l@l_or)
	       | _ -> assert false
	  ) l_and 
    in
    Comb (And, l)

  let rec flatten_or = function
    | [] -> []
    | Comb (Or, l)::r -> l@(flatten_or r)
    | Lit a :: r -> (Lit a)::(flatten_or r)
    | _ -> assert false
    
  let rec flatten_and = function
    | [] -> []
    | Comb (And, l)::r -> l@(flatten_and r)
    | a :: r -> a::(flatten_and r)
    

  let rec cnf f = 
    match f with
      | Comb (Or, l) -> 
	  begin
	    let l = List.map cnf l in
	    let l_and, l_or = 
	      List.partition (function Comb(And,_) -> true | _ -> false) l in
	    match l_and with
	      | [ Comb(And, l_conj) ] -> 
		  let u = flatten_or l_or in
		  distrib l_conj u

	      | Comb(And, l_conj) :: r ->
		  let u = flatten_or l_or in
		  cnf (Comb(Or, (distrib l_conj u)::r))

	      | _ ->  
		  begin
		    match flatten_or l_or with
		      | [] -> assert false
		      | [r] -> r
		      | v -> Comb (Or, v)
		  end
	  end
      | Comb (And, l) -> 
	  Comb (And, List.map cnf l)
      | f -> f    


  let ( @@ ) l1 l2 = List.rev_append l1 l2

  let rec mk_cnf = function
    | Comb (And, l) ->
      List.fold_left (fun acc f ->  (mk_cnf f) @@ acc) [] l
	
    | Comb (Or, [f1;f2]) ->
      let ll1 = mk_cnf f1 in
      let ll2 = mk_cnf f2 in
      List.fold_left 
	(fun acc l1 -> (List.rev_map (fun l2 -> l1 @@ l2)ll2) @@ acc) [] ll1

    | Comb (Or, f1 :: l) ->
      let ll1 = mk_cnf f1 in
      let ll2 = mk_cnf (Comb (Or, l)) in
      List.fold_left 
	(fun acc l1 -> (List.rev_map (fun l2 -> l1 @@ l2)ll2) @@ acc) [] ll1

    | Lit a -> [[a]]
    | Comb (Not, [Lit a]) -> [[Literal.LT.neg a]]
    | _ -> assert false


  let rec unfold mono f = 
    match f with
      | Lit a -> a::mono 
      | Comb (Not, [Lit a]) -> 
	  (Literal.LT.neg a)::mono
      | Comb (Or, l) -> 
	  List.fold_left unfold mono l
      | _ -> assert false
	  
  let rec init monos f = 
    match f with
      | Comb (And, l) -> 
	  List.fold_left init monos l
      | f -> (unfold [] f)::monos
	
  let make_cnf f =
    let sfnc = cnf (sform f) in
    init [] sfnc

  let mk_proxy =  
    let cpt = ref 0 in
    fun () ->      
      let t = AETerm.make 
        (Symbols.name (Hstring.make ("PROXY__"^(string_of_int !cpt))))
        [] Ty.Tbool
      in
      incr cpt;
      Literal.LT.make (Literal.Eq (t, AETerm.vrai))

  module Tseitin (Dummy : sig end)= struct
    let acc_or = ref []
    let acc_and = ref []

    let rec cnf f = match f with
      | Lit a -> None, [a]
      | Comb (Not, [Lit a]) -> None, [Literal.LT.neg a]

      | Comb (And, l) ->
          List.fold_left
            (fun (_, acc) f ->
              match cnf f with
                | _, [] -> assert false
                | cmb, [a] -> cmb, a :: acc
                | Some And, l ->
                    Some And, l @ acc
                    (* let proxy = mk_proxy () in *)
                    (* acc_and := (proxy, l) :: !acc_and; *)
                    (* proxy :: acc *)
                | Some Or, l ->
                    let proxy = mk_proxy () in
                    acc_or := (proxy, l) :: !acc_or;
                    Some And, proxy :: acc
                | _ -> assert false
            ) (None, []) l

      | Comb (Or, l) ->
          List.fold_left
            (fun (_, acc) f ->
              match cnf f with
                | _, [] -> assert false
                | cmb, [a] -> cmb, a :: acc
                | Some Or, l ->
                    Some Or, l @ acc
                    (* let proxy = mk_proxy () in *)
                    (* acc_or := (proxy, l) :: !acc_or; *)
                    (* proxy :: acc *)
                | Some And, l ->
                    let proxy = mk_proxy () in
                    acc_and := (proxy, l) :: !acc_and;
                    Some Or, proxy :: acc
                | _ -> assert false
            ) (None, []) l
            
      | _ -> assert false
          
    let cnf f =
      let acc = match f with
        | Comb (And, l) -> List.rev_map (fun f -> snd(cnf f)) l
        | _ -> [snd (cnf f)]
      in
      let proxies = ref [] in
      let acc =
        List.fold_left
          (fun acc (p,l) ->
            proxies := p :: !proxies;
            let np = Literal.LT.neg p in
            let cl, acc =
              List.fold_left
                (fun (cl,acc) a -> (Literal.LT.neg a :: cl), [np; a] :: acc)
                ([p],acc) l in
            cl :: acc
          )acc !acc_and
      in
      let acc =
        List.fold_left
          (fun acc (p,l) ->
            proxies := p :: !proxies;
            let acc = List.fold_left (fun acc a -> [p; Literal.LT.neg a]::acc)
              acc l in
            (Literal.LT.neg p :: l) :: acc
          ) acc !acc_or
      in
      acc

    let make_cnf f =
      acc_or := [];
      acc_and := [];
      cnf (sform f)

    (* Naive CNF *)
    let make_cnf f = mk_cnf (sform f)
  end

end

exception Unsat of int list

let set_cc b = Cc.cc_active := b

module type Solver = sig
  type state

  val get_time : unit -> float
  val get_calls : unit -> int

  val clear : unit -> unit
  val assume : ?profiling:bool -> id:int -> Formula.t -> unit
  val check : ?profiling:bool -> unit -> unit

  val save_state : unit -> state
  val restore_state : state -> unit
  val entails : ?profiling:bool -> id:int -> Formula.t -> bool
end

module Make (Dummy : sig end) = struct

  let calls = ref 0
  module Time = Timer.Make (Dummy)

  let get_time = Time.get
  let get_calls () = !calls

  module Tseitin = Formula.Tseitin (Dummy)
  module CSolver = Solver.Make (Dummy)

  let clear () = CSolver.clear ()

  let check_unsatcore uc =
    eprintf "Unsat Core : @.";
    List.iter 
      (fun c -> 
        eprintf "%a@." (Formula.print_list "or") 
          (List.map (fun x -> Formula.Lit x) c)) uc;
    eprintf "@.";
    try 
      clear ();
      CSolver.assume uc 0;
      CSolver.solve ();
      eprintf "Not an unsat core !!!@.";
      assert false
    with 
      | Solver.Unsat _ -> ();
      | Solver.Sat  -> 
          eprintf "Sat: Not an unsat core !!!@.";
          assert false

  let export_unsatcore cl = 
    let uc = List.map (fun {Solver_types.atoms=atoms} ->
      let l = ref [] in
      for i = 0 to Vec.size atoms - 1 do
        l := (Vec.get atoms i).Solver_types.lit :: !l
      done; 
      !l) cl
    in (* check_unsatcore uc; *)
    uc

  module SInt = 
    Set.Make (struct type t = int let compare = Pervasives.compare end)

  let export_unsatcore2 cl =
    let s = 
      List.fold_left 
        (fun s {Solver_types.name = n} ->
	  try SInt.add (int_of_string n) s with _ -> s) SInt.empty cl
    in 
    SInt.elements s

  let assume ?(profiling = false) ~id f = 
    if profiling then Time.start ();
    try 
      CSolver.assume (Tseitin.make_cnf f) id;
      if profiling then Time.pause ()
    with Solver.Unsat ex ->
      if profiling then Time.pause ();
      raise (Unsat (export_unsatcore2 ex))

  let check ?(profiling = false) () =
    incr calls;
    if profiling then Time.start ();
    try 
      CSolver.solve ();
      if profiling then Time.pause ()
    with
      | Solver.Sat -> if profiling then Time.pause ()
      | Solver.Unsat ex -> 
	  if profiling then Time.pause ();
	  raise (Unsat (export_unsatcore2 ex))
            
  type state = CSolver.state

  let save_state = CSolver.save

  let restore_state = CSolver.restore

  let entails ?(profiling=false) ~id f =
    let st = save_state () in
    let ans = 
      try
        assume ~profiling ~id (Formula.make Formula.Not [f]) ;
        check ~profiling ();
        false
      with Unsat _ -> true
    in
    restore_state st;
    ans

end
