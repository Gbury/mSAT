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


(* Solver types for McSat Solving *)
(* ************************************************************************ *)

module McMake (L : Log_intf.S)(E : Expr_intf.S)(Th : Plugin_intf.S with
    type formula = E.Formula.t and type term = E.Term.t) = struct

  (* Flag for Mcsat v.s Pure Sat *)
  let mcsat = true

  (* Types declarations *)

  type term = E.Term.t
  type formula = E.Formula.t
  type proof = Th.proof

  type lit = {
    lid : int;
    term : term;
    mutable level : int;
    mutable weight : float;
    mutable assigned : term option;
  }

  type var = {
    vid : int;
    pa : atom;
    na : atom;
    mutable seen : bool;
    mutable level : int;
    mutable weight : float;
    mutable reason : reason;
  }

  and atom = {
    aid : int;
    var : var;
    neg : atom;
    lit : formula;
    mutable is_true : bool;
    mutable watched : clause Vec.t;
  }

  and clause = {
    name : string;
    tag : int option;
    atoms : atom Vec.t;
    learnt : bool;
    cpremise : premise;
    mutable activity : float;
    mutable removed : bool;
  }

  and reason =
    | Semantic of int
    | Bcp of clause option

  and premise =
    | History of clause list
    | Lemma of proof

  type elt = (lit, var) Either.t

  (* Dummy values *)
  let dummy_lit = E.dummy

  let rec dummy_var =
    { vid = -101;
      pa = dummy_atom;
      na = dummy_atom;
      seen = false;
      level = -1;
      weight = -1.;
      reason = Bcp None;
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
      tag = None;
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

  let f_map = MF.create 4096
  let t_map = MT.create 4096

  let vars = Vec.make 107 (Either.mk_right dummy_var)
  let nb_elt () = Vec.size vars
  let get_elt i = Vec.get vars i
  let iter_elt f = Vec.iter f vars

  let cpt_mk_var = ref 0

  let make_semantic_var t =
    try MT.find t_map t
    with Not_found ->
      let res = {
        lid = !cpt_mk_var;
        term = t;
        weight = 1.;
        level = -1;
        assigned = None;
      } in
      incr cpt_mk_var;
      MT.add t_map t res;
      Vec.push vars (Either.mk_left res);
      res

  let make_boolean_var =
    fun lit ->
      let lit, negated = E.norm lit in
      try MF.find f_map lit, negated
      with Not_found ->
        let cpt_fois_2 = !cpt_mk_var lsl 1 in
        let rec var  =
          { vid = !cpt_mk_var;
            pa = pa;
            na = na;
            seen = false;
            level = -1;
            weight = 0.;
            reason = Bcp None;
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
        MF.add f_map lit var;
        incr cpt_mk_var;
        Vec.push vars (Either.mk_right var);
        Th.iter_assignable (fun t -> ignore (make_semantic_var t)) lit;
        var, negated

  let add_term t = make_semantic_var t

  let add_atom lit =
    let var, negated = make_boolean_var lit in
    if negated then var.na else var.pa

  let make_clause ?tag name ali sz_ali is_learnt premise =
    let atoms = Vec.from_list ali sz_ali dummy_atom in
    { name  = name;
      tag = tag;
      atoms = atoms;
      removed = false;
      learnt = is_learnt;
      activity = 0.;
      cpremise = premise}

  let empty_clause = make_clause "Empty" [] 0 false (History [])

  (* Decisions & propagations *)
  type t = (lit, atom) Either.t

  let of_lit = Either.mk_left
  let of_atom = Either.mk_right
  let destruct = Either.destruct

  (* Elements *)
  let elt_of_lit = Either.mk_left
  let elt_of_var = Either.mk_right

  let destruct_elt = Either.destruct

  let get_elt_id = function
    | Either.Left l -> l.lid | Either.Right v ->  v.vid
  let get_elt_level = function
    | Either.Left (l :lit) -> l.level | Either.Right v ->  v.level
  let get_elt_weight = function
    | Either.Left (l : lit) -> l.weight | Either.Right v ->  v.weight

  let set_elt_level e lvl = match e with
    | Either.Left (l : lit) -> l.level <- lvl | Either.Right v ->  v.level <- lvl
  let set_elt_weight e w = match e with
    | Either.Left (l : lit) -> l.weight <- w | Either.Right v ->  v.weight <- w

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
      Th.iter_assignable (fun t -> l := add_term t :: !l) v.pa.lit;
      iter_map := Mi.add v.vid !l !iter_map;
      List.iter f !l

  (* Proof debug info *)
  let proof_debug p =
    let name, l, l', color = Th.proof_debug p in
    name, (List.map add_atom l), (List.map add_term l'), color

  (* Pretty printing for atoms and clauses *)
  let print_lit fmt v = E.Term.print fmt v.term

  let print_atom fmt a = E.Formula.print fmt a.lit

  let print_atoms fmt v =
    if Vec.size v = 0 then
      Format.fprintf fmt "∅"
    else begin
      print_atom fmt (Vec.get v 0);
      if (Vec.size v) > 1 then begin
        for i = 1 to (Vec.size v) - 1 do
          Format.fprintf fmt " ∨ %a" print_atom (Vec.get v i)
        done
      end
    end

  let print_clause fmt c =
    Format.fprintf fmt "%s : %a" c.name print_atoms c.atoms

  (* Complete debug printing *)
  let sign a = if a == a.var.pa then "" else "-"

  let level a =
    match a.var.level, a.var.reason with
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

  let pp_lit b v =
    bprintf b "%d [lit:%s]%a"
      (v.lid+1) (Log.on_fmt E.Term.print v.term) pp_assign v.assigned

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



(* Solver types for pure SAT Solving *)
(* ************************************************************************ *)



module SatMake (L : Log_intf.S)(E : Formula_intf.S)
    (Th : Theory_intf.S with type formula = E.t ) = struct

  (* Flag for Mcsat v.s Pure Sat *)
  let mcsat = false

  (* Types declarations *)

  type term = E.t
  type formula = E.t
  type proof = Th.proof

  type lit = {
    lid : int;
    term : term;
    mutable level : int;
    mutable weight : float;
    mutable assigned : term option;
  }

  type var = {
    vid : int;
    pa : atom;
    na : atom;
    mutable seen : bool;
    mutable level : int;
    mutable weight : float;
    mutable reason : reason;
  }

  and atom = {
    aid : int;
    var : var;
    neg : atom;
    lit : formula;
    mutable is_true : bool;
    mutable watched : clause Vec.t;
  }

  and clause = {
    name : string;
    tag : int option;
    atoms : atom Vec.t;
    learnt : bool;
    cpremise : premise;
    mutable activity : float;
    mutable removed : bool;
  }

  and reason =
    | Semantic of int
    | Bcp of clause option

  and premise =
    | History of clause list
    | Lemma of proof

  type elt = var

  (* Dummy values *)
  let dummy_lit = E.dummy

  let rec dummy_var =
    { vid = -101;
      pa = dummy_atom;
      na = dummy_atom;
      seen = false;
      level = -1;
      weight = -1.;
      reason = Bcp None;
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
      tag = None;
      atoms = Vec.make_empty dummy_atom;
      activity = -1.;
      removed = false;
      learnt = false;
      cpremise = History [] }

  let () =
    dummy_atom.watched <- Vec.make_empty dummy_clause

  (* Constructors *)
  module MF = Hashtbl.Make(E)

  let f_map = MF.create 4096

  let vars = Vec.make 107 dummy_var
  let nb_elt () = Vec.size vars
  let get_elt i = Vec.get vars i
  let iter_elt f = Vec.iter f vars

  let cpt_mk_var = ref 0

  let make_semantic_var _ = assert false

  let make_boolean_var =
    fun lit ->
      let lit, negated = E.norm lit in
      try MF.find f_map lit, negated
      with Not_found ->
        let cpt_fois_2 = !cpt_mk_var lsl 1 in
        let rec var  =
          { vid = !cpt_mk_var;
            pa = pa;
            na = na;
            seen = false;
            level = -1;
            weight = 0.;
            reason = Bcp None;
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
        MF.add f_map lit var;
        incr cpt_mk_var;
        Vec.push vars var;
        var, negated

  let add_term t = make_semantic_var t

  let add_atom lit =
    let var, negated = make_boolean_var lit in
    if negated then var.na else var.pa

  let make_clause ?tag name ali sz_ali is_learnt premise =
    let atoms = Vec.from_list ali sz_ali dummy_atom in
    { name  = name;
      tag = tag;
      atoms = atoms;
      removed = false;
      learnt = is_learnt;
      activity = 0.;
      cpremise = premise}

  let empty_clause = make_clause "Empty" [] 0 false (History [])

  (* Decisions & propagations *)
  type t = atom

  let of_lit _ = assert false
  let of_atom a = a
  let destruct e _ f = f e

  (* Elements *)
  let elt_of_lit _ = assert false
  let elt_of_var v = v

  let destruct_elt v _ f = f v

  let get_elt_id v = v.vid
  let get_elt_level v = v.level
  let get_elt_weight v = v.weight

  let set_elt_level v lvl = v.level <- lvl
  let set_elt_weight v w = v.weight <- w

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
  let iter_sub _ _ = ()

  (* Proof debug info *)
  let proof_debug _ = "lemma", [], [], None

  (* Pretty printing for atoms and clauses *)
  let print_lit _ _ = assert false

  let print_atom fmt a = E.print fmt a.lit

  let print_atoms fmt v =
    if Vec.size v = 0 then
      Format.fprintf fmt "∅"
    else begin
      print_atom fmt (Vec.get v 0);
      if (Vec.size v) > 1 then begin
        for i = 1 to (Vec.size v) - 1 do
          Format.fprintf fmt " ∨ %a" print_atom (Vec.get v i)
        done
      end
    end

  let print_clause fmt c =
    Format.fprintf fmt "%s : %a" c.name print_atoms c.atoms

  (* Complete debug printing *)
  let sign a = if a == a.var.pa then "" else "-"

  let level a =
    match a.var.level, a.var.reason with
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

  let pp_assign _ _ = ()

  let pp_lit b v = bprintf b "%d [lit:()]" (v.lid+1)

  let pp_atom b a =
    bprintf b "%s%d%s[lit:%s]"
      (sign a) (a.var.vid+1) (value a) (Log.on_fmt E.print a.lit)

  let pp_atoms_vec b vec =
    for i = 0 to Vec.size vec - 1 do
      bprintf b "%a ; " pp_atom (Vec.get vec i)
    done

  let pp_clause b {name=name; atoms=arr; cpremise=cp; learnt=learnt} =
    bprintf b "%s%s{ %a} cpremise={{%a}}" name (if learnt then "!" else ":") pp_atoms_vec arr pp_premise cp

end
