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

open Printf

module type S = Mcsolver_types_intf.S

module Make (L : Log_intf.S)(E : Expr_intf.S)(Th : Plugin_intf.S with
    type formula = E.Formula.t and type term = E.Term.t) = struct

  (* Types declarations *)

  type term = E.Term.t
  type formula = E.Formula.t
  type proof = Th.proof

  type 'a var =
    { vid : int;
      tag : 'a;
      mutable weight : float;
      mutable level : int; }

  type semantic =
    { term : term;
      mutable assigned : term option; }

  type boolean = {
    pa : atom;
    na : atom;
    mutable reason : reason;
  }

  and atom =
    { var : boolean var;
      lit : formula;
      neg : atom;
      mutable watched : clause Vec.t;
      mutable is_true : bool;
      aid : int }

  and clause =
    { name : string;
      atoms : atom Vec.t ;
      mutable activity : float;
      mutable removed : bool;
      learnt : bool;
      cpremise : premise }

  and reason =
      | Semantic of int
      | Bcp of clause option

  and premise =
      | History of clause list
      | Lemma of proof

  type elt = (semantic var, boolean var) Either.t

  (* Dummy values *)
  let dummy_lit = E.dummy

  let rec dummy_var =
    { vid = -101;
      level = -1;
      weight = -1.;
      tag = {
        pa = dummy_atom;
        na = dummy_atom;
        reason = Bcp None; };
    }
  and dummy_atom =
    { var = dummy_var;
      lit = dummy_lit;
      watched = Obj.magic 0;
      (* should be [Vec.make_empty dummy_clause]
         but we have to break the cycle *)
      neg = dummy_atom;
      is_true = false;
      aid = -102 }
  let dummy_clause =
    { name = "";
      atoms = Vec.make_empty dummy_atom;
      activity = -1.;
      removed = false;
      learnt = false;
      cpremise = History [] }

  let () =
    dummy_atom.watched <- Vec.make_empty dummy_clause

  (* Constructors *)
  module MF = Hashtbl.Make(E.Formula)
  module MT = Hashtbl.Make(E.Term)

  let f_map = MF.create 1007
  let t_map = MT.create 1007

  let vars = Vec.make 107 (Either.mk_right dummy_var)
  let nb_vars () = Vec.size vars
  let get_var i = Vec.get vars i
  let iter_vars f = Vec.iter f vars

  let cpt_mk_var = ref 0

  let make_semantic_var t =
      try MT.find t_map t
      with Not_found ->
        let res = {
          vid = !cpt_mk_var;
          weight = 1.;
          level = -1;
          tag = {
            term = t;
            assigned = None; };
        } in
        incr cpt_mk_var;
        MT.add t_map t res;
        Vec.push vars (Either.mk_left res);
        res

  let make_boolean_var =
    fun lit ->
      L.debug 100 "normalizing lit";
      let lit, negated = E.norm lit in
      try MF.find f_map lit, negated
      with Not_found ->
        L.debug 100 "Lit not in table";
        let cpt_fois_2 = !cpt_mk_var lsl 1 in
        let rec var  =
          { vid = !cpt_mk_var;
            level = -1;
            weight = 0.;
            tag = {
              pa = pa;
              na = na;
              reason = Bcp None;};
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
            lit = E.neg lit;
            watched = Vec.make 10 dummy_clause;
            neg = pa;
            is_true = false;
            aid = cpt_fois_2 + 1 (* aid = vid*2+1 *) } in
        L.debug 100 "adding lit to table...";
        MF.add f_map lit var;
        L.debug 100 "done";
        incr cpt_mk_var;
        Vec.push vars (Either.mk_right var);
        L.debug 100 "iterating on new lit...";
        Th.iter_assignable (fun t -> ignore (make_semantic_var t)) lit;
        L.debug 100 "done";
        var, negated

  let add_term t = make_semantic_var t

  let add_atom lit =
    Log.debug 100 "entering add_atom";
    let var, negated = make_boolean_var lit in
    Log.debug 100 "found atom";
    if negated then var.tag.na else var.tag.pa

  let make_clause name ali sz_ali is_learnt premise =
    let atoms = Vec.from_list ali sz_ali dummy_atom in
    { name  = name;
      atoms = atoms;
      removed = false;
      learnt = is_learnt;
      activity = 0.;
      cpremise = premise}

  let empty_clause = make_clause "Empty" [] 0 false (History [])

  (* Name generation *)
  let fresh_lname =
    let cpt = ref 0 in
    fun () -> incr cpt; "L" ^ (string_of_int !cpt)

  let fresh_hname =
    let cpt = ref 0 in
    fun () -> incr cpt; "H" ^ (string_of_int !cpt)

  let fresh_tname =
    let cpt = ref 0 in
    fun () -> incr cpt; "T" ^ (string_of_int !cpt)

  let fresh_name =
    let cpt = ref 0 in
    fun () -> incr cpt; "C" ^ (string_of_int !cpt)

  (* Iteration over subterms *)
  module Mi = Map.Make(struct type t = int let compare= Pervasives.compare end)
  let iter_map = ref Mi.empty

  let iter_sub f v =
      try
          List.iter f (Mi.find v.vid !iter_map)
      with Not_found ->
          let l = ref [] in
          Th.iter_assignable (fun t -> l := add_term t :: !l) v.tag.pa.lit;
          iter_map := Mi.add v.vid !l !iter_map;
          List.iter f !l

  (* Proof debug info *)
  let proof_debug p =
      let name, l, l', color = Th.proof_debug p in
      name, (List.map add_atom l), (List.map add_term l'), color

  (* Pretty printing for atoms and clauses *)
  let print_semantic_var fmt v = E.Term.print fmt v.tag.term

  let print_atom fmt a = E.Formula.print fmt a.lit

  let print_atoms fmt v =
    print_atom fmt (Vec.get v 0);
    if (Vec.size v) > 1 then begin
      for i = 1 to (Vec.size v) - 1 do
        Format.fprintf fmt " ∨ %a" print_atom (Vec.get v i)
      done
    end

  let print_clause fmt c =
    Format.fprintf fmt "%s : %a" c.name print_atoms c.atoms

  (* Complete debug printing *)
  let sign a = if a==a.var.tag.pa then "" else "-"

  let level a =
    match a.var.level, a.var.tag.reason with
    | n, _ when n < 0 -> assert false
    | 0, Bcp (Some c) -> sprintf "->0/%s" c.name
    | 0, Bcp None   -> "@0"
    | n, Bcp (Some c) -> sprintf "->%d/%s" n c.name
    | n, Bcp None   -> sprintf "@@%d" n
    | n, Semantic lvl -> sprintf "::%d/%d" n lvl

  let value a =
    if a.is_true then sprintf "[T%s]" (level a)
    else if a.neg.is_true then sprintf "[F%s]" (level a)
    else "[]"

  let pp_premise b = function
    | History v -> List.iter (fun {name=name} -> bprintf b "%s," name) v
    | Lemma _ -> bprintf b "th_lemma"

  let pp_assign b = function
      | None -> ()
      | Some t -> bprintf b "[assignment: %s]" (Log.on_fmt E.Term.print t)

  let pp_semantic_var b v =
    bprintf b "%d [lit:%s]%a"
      (v.vid+1) (Log.on_fmt E.Term.print v.tag.term) pp_assign v.tag.assigned

  let pp_atom b a =
    bprintf b "%s%d%s[lit:%s]"
      (sign a) (a.var.vid+1) (value a) (Log.on_fmt E.Formula.print a.lit)

  let pp_atoms_vec b vec =
    for i = 0 to Vec.size vec - 1 do
      bprintf b "%a ; " pp_atom (Vec.get vec i)
    done

  let pp_clause b {name=name; atoms=arr; cpremise=cp; learnt=learnt} =
    bprintf b "%s%s{ %a} cpremise={{%a}}" name (if learnt then "!" else ":") pp_atoms_vec arr pp_premise cp

end
