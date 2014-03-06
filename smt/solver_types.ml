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

open Format

let ale = Hstring.make "<=" 
let alt = Hstring.make "<"
let agt = Hstring.make ">"

let is_le n = Hstring.compare n ale = 0
let is_lt n = Hstring.compare n alt = 0
let is_gt n = Hstring.compare n agt = 0


type var =
    {  vid : int;
       pa : atom;
       na : atom;
       mutable weight : float;
       mutable seen : bool;
       mutable level : int;
       mutable reason: reason;
       mutable vpremise : premise}
      
and atom = 
    { var : var;
      lit : Literal.LT.t;
      neg : atom;
      mutable watched : clause Vec.t;
      mutable is_true : bool;
      aid : int }

and clause = 
    { name : string; 
      mutable atoms : atom Vec.t ; 
      mutable activity : float;
      mutable removed : bool;
      learnt : bool;
      cpremise : premise }

and reason = clause option

and premise = clause list

module Make (Dummy : sig end) = struct

let dummy_lit = Literal.LT.make (Literal.Eq(Term.vrai,Term.vrai))

let rec dummy_var =
  { vid = -101;
    pa = dummy_atom;
    na = dummy_atom; 
    level = -1;
    reason = None;
    weight = -1.;
    seen = false;
    vpremise = [] }
and dummy_atom = 
  { var = dummy_var; 
    lit = dummy_lit;
    watched = {Vec.dummy=dummy_clause; data=[||]; sz=0};
    neg = dummy_atom;
    is_true = false;
    aid = -102 }
and dummy_clause = 
  { name = ""; 
    atoms = {Vec.dummy=dummy_atom; data=[||]; sz=0};
    activity = -1.;
    removed = false;
    learnt = false;
    cpremise = [] }


module MA = Literal.LT.Map
  
let ale = Hstring.make "<=" 
let alt = Hstring.make "<"
let agt = Hstring.make ">"
let is_le n = Hstring.compare n ale = 0
let is_lt n = Hstring.compare n alt = 0
let is_gt n = Hstring.compare n agt = 0

let normal_form lit =
  match Literal.LT.view lit with
    | Literal.Eq (t1,t2)  when Term.equal t2 Term.faux ->
      Literal.LT.make (Literal.Eq(t1,Term.vrai)), true

    | Literal.Eq (t1,t2)  when Term.equal t1 Term.faux ->
      Literal.LT.make (Literal.Eq(t2,Term.vrai)), true


    | Literal.Distinct(false, [t1;t2])  when Term.equal t1 Term.faux ->
      Literal.LT.make (Literal.Eq(t2,Term.vrai)), false

    | Literal.Distinct(false, [t1;t2])  when Term.equal t2 Term.faux ->
      Literal.LT.make (Literal.Eq(t1,Term.vrai)), false

    | Literal.Distinct(false, [t1;t2])  when Term.equal t1 Term.vrai ->
      Literal.LT.make (Literal.Eq(t2,Term.vrai)), true

    | Literal.Distinct(false, [t1;t2])  when Term.equal t2 Term.vrai ->
      Literal.LT.make (Literal.Eq(t1,Term.vrai)), true

    | Literal.Distinct(false,[_;_]) -> Literal.LT.neg lit, true

    | Literal.Builtin(true,n,[t1;t2]) when is_gt n ->
      Literal.LT.neg lit, true

    | Literal.Builtin(false,n,[t1;t2]) when is_le n ->
      Literal.LT.neg lit, true
    | _ -> lit, false


(* let normal_form lit = *)
(*   match Literal.LT.view lit with *)
(*     | Literal.Eq (t1,t2)  -> *)
(*         if Term.equal t2 Term.faux || Term.equal t1 Term.faux then *)
(*           Literal.LT.neg lit, true *)
(*         else *)
(*           lit, false *)

(*     | Literal.Distinct(false,[_;_]) -> Literal.LT.neg lit, true *)
(*     | Literal.Builtin(true,n,[t1;t2]) when Builtin.is_gt n -> *)
(*   Literal.LT.neg lit, true *)
    
(*     | Literal.Builtin(false,n,[t1;t2]) when Builtin.is_le n -> *)
(*   Literal.LT.neg lit, true *)
(*     | _ -> lit, false *)


let cpt_mk_var = ref 0
let ma = ref MA.empty
let make_var =
  fun lit ->
    let lit, negated = normal_form lit in
    try MA.find lit !ma, negated 
    with Not_found ->
      let cpt_fois_2 = !cpt_mk_var lsl 1 in
      let rec var  = 
	{ vid = !cpt_mk_var; 
	  pa = pa;
	  na = na; 
	  level = -1;
	  reason = None;
	  weight = 0.;
	  seen = false;
	  vpremise = [];
	}
      and pa = 
	{ var = var; 
	  lit = lit;
	  watched = Vec.make 10 dummy_clause; 
	  neg = na;
	  is_true = false;
	  aid = cpt_fois_2 (* aid = vid*2 *) }
      and na = 
	{ var = var;
	  lit = Literal.LT.neg lit;
	  watched = Vec.make 10 dummy_clause;
	  neg = pa;
	  is_true = false;
	  aid = cpt_fois_2 + 1 (* aid = vid*2+1 *) } in 
      ma := MA.add lit var !ma;
      incr cpt_mk_var;
      var, negated

let made_vars_info () = !cpt_mk_var, MA.fold (fun lit var acc -> var::acc)!ma []

let add_atom lit =
  let var, negated = make_var lit in
  if negated then var.na else var.pa 

let make_clause name ali sz_ali is_learnt premise = 
  let atoms = Vec.from_list ali sz_ali dummy_atom in
  { name  = name;
    atoms = atoms;
    removed = false;
    learnt = is_learnt;
    activity = 0.;
    cpremise = premise}
    
let fresh_lname =
  let cpt = ref 0 in 
  fun () -> incr cpt; "L" ^ (string_of_int !cpt)

let fresh_dname =
  let cpt = ref 0 in 
  fun () -> incr cpt; "D" ^ (string_of_int !cpt)
    
let fresh_name =
  let cpt = ref 0 in 
  fun () -> incr cpt; "C" ^ (string_of_int !cpt)



module Clause = struct
      
  let size c = Vec.size c.atoms
  let pop c = Vec.pop c.atoms
  let shrink c i = Vec.shrink  c.atoms i
  let last c = Vec.last c.atoms
  let get c i = Vec.get c.atoms i
  let set c i v = Vec.set c.atoms i v

end

let to_float i = float_of_int i

let to_int f = int_of_float f

let clear () =
  cpt_mk_var := 0;
  ma := MA.empty

end



module Debug = struct
    
  let sign a = if a==a.var.pa then "" else "-"
      
  let level a =
    match a.var.level, a.var.reason with 
      | n, _ when n < 0 -> assert false
      | 0, Some c -> sprintf "->0/%s" c.name
      | 0, None   -> "@0"
      | n, Some c -> sprintf "->%d/%s" n c.name
      | n, None   -> sprintf "@@%d" n

  let value a = 
    if a.is_true then sprintf "[T%s]" (level a)
    else if a.neg.is_true then sprintf "[F%s]" (level a)
    else ""

  let value_ms_like a = 
    if a.is_true then sprintf ":1%s" (level a)
    else if a.neg.is_true then sprintf ":0%s" (level a)
    else ":X"

  let premise fmt v = 
    List.iter (fun {name=name} -> fprintf fmt "%s," name) v

  let atom fmt a = 
    fprintf fmt "%s%d%s [lit:%a] vpremise={{%a}}" 
      (sign a) (a.var.vid+1) (value a) Literal.LT.print a.lit
      premise a.var.vpremise


  let atoms_list fmt l = List.iter (fprintf fmt "%a ; " atom) l
  let atoms_array fmt arr = Array.iter (fprintf fmt "%a ; " atom) arr

  let atoms_vec fmt vec = 
    for i = 0 to Vec.size vec - 1 do
      fprintf fmt "%a ; " atom (Vec.get vec i)
    done

  let clause fmt {name=name; atoms=arr; cpremise=cp} =
    fprintf fmt "%s:{ %a} cpremise={{%a}}" name atoms_vec arr premise cp



end
