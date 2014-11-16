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

module type S = Solver_types_intf.S

module Make (F : Formula_intf.S)(Th : Theory_intf.S) = struct

  type formula = F.t
  type proof = Th.proof

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

  and reason = clause option

  and premise =
      | History of clause list
      | Lemma of proof

  let dummy_lit = F.dummy

  let rec dummy_var =
    { vid = -101;
      pa = dummy_atom;
      na = dummy_atom;
      level = -1;
      reason = None;
      weight = -1.;
      seen = false;
      vpremise = History [] }
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

  module MA = Map.Make(F)

  let normal_form = F.norm

  let ma = ref MA.empty
  let vars = Vec.make 107 dummy_var

  let nb_vars () = Vec.size vars
  let get_var i = Vec.get vars i
  let iter_vars f = Vec.iter f vars

  let cpt_mk_var = ref 0
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
            vpremise = History [];
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
            lit = F.neg lit;
            watched = Vec.make 10 dummy_clause;
            neg = pa;
            is_true = false;
            aid = cpt_fois_2 + 1 (* aid = vid*2+1 *) } in
        ma := MA.add lit var !ma;
        incr cpt_mk_var;
        Vec.push vars var;
        assert (Vec.get vars var.vid == var && !cpt_mk_var = Vec.size vars);
        var, negated

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

  let empty_clause = make_clause "Empty" [] 0 false (History [])

  let fresh_lname =
    let cpt = ref 0 in
    fun () -> incr cpt; "L" ^ (string_of_int !cpt)

  let fresh_dname =
    let cpt = ref 0 in
    fun () -> incr cpt; "D" ^ (string_of_int !cpt)

  let fresh_name =
    let cpt = ref 0 in
    fun () -> incr cpt; "C" ^ (string_of_int !cpt)

  let clear () =
    cpt_mk_var := 0;
    ma := MA.empty

  (* Pretty printing for atoms and clauses *)
  let print_atom fmt a = F.print fmt a.lit

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

  let pp_premise b = function
      | History v -> List.iter (fun {name=name} -> bprintf b "%s," name) v
      | Lemma _ -> bprintf b "th_lemma"

  let pp_atom b a =
    bprintf b "%s%d%s [lit:%s] vpremise={{%a}}"
      (sign a) (a.var.vid+1) (value a) (Log.on_fmt F.print a.lit)
      pp_premise a.var.vpremise

  let pp_atoms_vec b vec =
    for i = 0 to Vec.size vec - 1 do
      bprintf b "%a ; " pp_atom (Vec.get vec i)
    done

  let pp_clause b {name=name; atoms=arr; cpremise=cp} =
    bprintf b "%s:{ %a} cpremise={{%a}}" name pp_atoms_vec arr pp_premise cp

end
