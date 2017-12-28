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
(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2016 Guillaume Bury
Copyright 2016 Simon Cruanes
*)

module type S = Solver_types_intf.S

module Var_fields = Solver_types_intf.Var_fields

let v_field_seen_neg = Var_fields.mk_field()
let v_field_seen_pos = Var_fields.mk_field()
let () = Var_fields.freeze()

(* Solver types for McSat Solving *)
(* ************************************************************************ *)

module McMake (E : Expr_intf.S)() = struct

  (* Flag for Mcsat v.s Pure Sat *)
  let mcsat = true

  type term = E.Term.t
  type formula = E.Formula.t
  type proof = E.proof

  type seen =
    | Nope
    | Both
    | Positive
    | Negative

  type lit = {
    lid : int;
    term : term;
    mutable l_level : int;
    mutable l_idx: int;
    mutable l_weight : float;
    mutable assigned : term option;
  }

  type var = {
    vid : int;
    pa : atom;
    na : atom;
    mutable v_fields : Var_fields.t;
    mutable v_level : int;
    mutable v_idx: int; (** position in heap *)
    mutable v_weight : float; (** Weight (for the heap), tracking activity *)
    mutable v_assignable: lit list option;
    mutable reason : reason option;
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
    atoms : atom array;
    mutable cpremise : premise;
    mutable activity : float;
    mutable attached : bool;
    mutable visited : bool;
  }

  and reason =
    | Decision
    | Bcp of clause
    | Semantic

  and premise =
    | Hyp
    | Local
    | Lemma of proof
    | History of clause list

  type elt =
    | E_lit of lit
    | E_var of var

  (* Dummy values *)
  let dummy_lit = E.Formula.dummy

  let rec dummy_var =
    { vid = -101;
      pa = dummy_atom;
      na = dummy_atom;
      v_fields = Var_fields.empty;
      v_level = -1;
      v_weight = -1.;
      v_idx= -1;
      v_assignable = None;
      reason = None;
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
      atoms = [| |];
      activity = -1.;
      attached = false;
      visited = false;
      cpremise = History [] }

  let () =
    dummy_atom.watched <- Vec.make_empty dummy_clause

  (* Constructors *)
  module MF = Hashtbl.Make(E.Formula)
  module MT = Hashtbl.Make(E.Term)

  let f_map = MF.create 4096
  let t_map = MT.create 4096

  let vars = Vec.make 107 (E_var dummy_var)
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
        l_weight = 1.;
        l_idx= -1;
        l_level = -1;
        assigned = None;
      } in
      incr cpt_mk_var;
      MT.add t_map t res;
      Vec.push vars (E_lit res);
      res

  let make_boolean_var : formula -> var * Expr_intf.negated =
    fun t ->
      let lit, negated = E.Formula.norm t in
      try
        MF.find f_map lit, negated
      with Not_found ->
        let cpt_fois_2 = !cpt_mk_var lsl 1 in
        let rec var  =
          { vid = !cpt_mk_var;
            pa = pa;
            na = na;
            v_fields = Var_fields.empty;
            v_level = -1;
            v_idx= -1;
            v_weight = 0.;
            v_assignable = None;
            reason = None;
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
            lit = E.Formula.neg lit;
            watched = Vec.make 10 dummy_clause;
            neg = pa;
            is_true = false;
            aid = cpt_fois_2 + 1 (* aid = vid*2+1 *) } in
        MF.add f_map lit var;
        incr cpt_mk_var;
        Vec.push vars (E_var var);
        var, negated

  let add_term t = make_semantic_var t

  let add_atom lit =
    let var, negated = make_boolean_var lit in
    match negated with
    | Formula_intf.Negated -> var.na
    | Formula_intf.Same_sign -> var.pa

  let make_clause ?tag name ali premise =
    let atoms = Array.of_list ali in
    { name  = name;
      tag = tag;
      atoms = atoms;
      attached = false;
      visited = false;
      activity = 0.;
      cpremise = premise}

  let empty_clause = make_clause "Empty" [] (History [])

  (* Marking helpers *)
  let clear v = v.v_fields <- Var_fields.empty

  let seen a =
    let pos = (a == a.var.pa) in
    let field = if pos then v_field_seen_pos else v_field_seen_neg in
    Var_fields.get field a.var.v_fields

  let seen_both v =
    Var_fields.get v_field_seen_pos v.v_fields &&
    Var_fields.get v_field_seen_neg v.v_fields

  let mark a =
    let pos = (a == a.var.pa) in
    let field = if pos then v_field_seen_pos else v_field_seen_neg in
    a.var.v_fields <- Var_fields.set field true a.var.v_fields

  (* Decisions & propagations *)
  type t =
    | Lit of lit
    | Atom of atom

  let of_lit l = Lit l
  let of_atom a = Atom a

  (* Elements *)
  let[@inline] elt_of_lit l = E_lit l
  let[@inline] elt_of_var v = E_var v

  let get_elt_id = function
    | E_lit l -> l.lid | E_var v ->  v.vid
  let get_elt_level = function
    | E_lit l -> l.l_level | E_var v ->  v.v_level
  let get_elt_idx = function
    | E_lit l -> l.l_idx | E_var v ->  v.v_idx
  let get_elt_weight = function
    | E_lit l -> l.l_weight | E_var v ->  v.v_weight

  let set_elt_level e lvl = match e with
    | E_lit l -> l.l_level <- lvl | E_var v ->  v.v_level <- lvl
  let set_elt_idx e i = match e with
    | E_lit l -> l.l_idx <- i | E_var v ->  v.v_idx <- i
  let set_elt_weight e w = match e with
    | E_lit l -> l.l_weight <- w | E_var v ->  v.v_weight <- w

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

  (* Pretty printing for atoms and clauses *)
  let print_lit fmt v = E.Term.print fmt v.term

  let print_atom fmt a = E.Formula.print fmt a.lit

  let print_atoms fmt v =
    if Array.length v = 0 then
      Format.fprintf fmt "∅"
    else begin
      print_atom fmt v.(0);
      if (Array.length v) > 1 then begin
        for i = 1 to (Array.length v) - 1 do
          Format.fprintf fmt " ∨ %a" print_atom v.(i)
        done
      end
    end

  let print_clause fmt c =
    Format.fprintf fmt "%s : %a" c.name print_atoms c.atoms

  (* Complete debug printing *)
  let sign a = if a == a.var.pa then "+" else "-"

  let pp_reason fmt = function
    | n, _ when n < 0 ->
      Format.fprintf fmt "%%"
    | n, None ->
      Format.fprintf fmt "%d" n
    | n, Some Decision ->
      Format.fprintf fmt "@@%d" n
    | n, Some Bcp c ->
      Format.fprintf fmt "->%d/%s" n c.name
    | n, Some Semantic ->
      Format.fprintf fmt "::%d" n

  let pp_level fmt a =
    pp_reason fmt (a.var.v_level, a.var.reason)

  let pp_value fmt a =
    if a.is_true then
      Format.fprintf fmt "T%a" pp_level a
    else if a.neg.is_true then
      Format.fprintf fmt "F%a" pp_level a
    else
      Format.fprintf fmt ""

  let pp_premise out = function
    | Hyp -> Format.fprintf out "hyp"
    | Local -> Format.fprintf out "local"
    | Lemma _ -> Format.fprintf out "th_lemma"
    | History v -> List.iter (fun { name; _ } -> Format.fprintf out "%s,@ " name) v

  let pp_assign fmt v =
    match v.assigned with
    | None ->
      Format.fprintf fmt ""
    | Some t ->
      Format.fprintf fmt "@[<hov>@@%d->@ %a@]" v.l_level E.Term.print t

  let pp_lit out v =
    Format.fprintf out "%d[%a][lit:@[<hov>%a@]]"
      (v.lid+1) pp_assign v E.Term.print v.term

  let pp_atom out a =
    Format.fprintf out "%s%d[%a][atom:@[<hov>%a@]]"
      (sign a) (a.var.vid+1) pp_value a E.Formula.print a.lit

  let pp_atoms_vec out vec =
    Array.iter (fun a -> Format.fprintf out "%a@ " pp_atom a) vec

  let pp_clause out {name=name; atoms=arr; cpremise=cp;_} =
    Format.fprintf out "%s@[<hov>{@[<hov>%a@]}@ cpremise={@[<hov>%a@]}@]"
      name pp_atoms_vec arr pp_premise cp

  let pp_dimacs fmt {atoms;_} =
    let aux fmt a =
      Array.iter (fun p ->
          Format.fprintf fmt "%s%d "
            (if p == p.var.pa then "-" else "")
            (p.var.vid+1)
        ) a
    in
    Format.fprintf fmt "%a0" aux atoms

  let pp fmt = function
    | Lit l -> pp_lit fmt l
    | Atom a -> pp_atom fmt a

end


(* Solver types for pure SAT Solving *)
(* ************************************************************************ *)

module SatMake (E : Formula_intf.S)() = struct
  include McMake(struct
      include E
      module Term = E
      module Formula = E
    end)(struct end)

  let mcsat = false
end

