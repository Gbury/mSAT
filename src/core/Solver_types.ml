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
    name : int;
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

  type trail_elt =
    | Lit of lit
    | Atom of atom

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
      lit = E.Formula.dummy;
      watched = Obj.magic 0;
      (* should be [Vec.make_empty dummy_clause]
         but we have to break the cycle *)
      neg = dummy_atom;
      is_true = false;
      aid = -102 }
  let dummy_clause =
    { name = -1;
      tag = None;
      atoms = [| |];
      activity = -1.;
      attached = false;
      visited = false;
      cpremise = History [] }

  let () = dummy_atom.watched <- Vec.make_empty dummy_clause

  (* Constructors *)
  module MF = Hashtbl.Make(E.Formula)
  module MT = Hashtbl.Make(E.Term)

  (* TODO: embed a state `t` with these inside *)
  let f_map = MF.create 4096
  let t_map = MT.create 4096

  let vars = Vec.make 107 (E_var dummy_var)
  let nb_elt () = Vec.size vars
  let get_elt i = Vec.get vars i
  let iter_elt f = Vec.iter f vars

  let cpt_mk_var = ref 0

  let name_of_clause c = match c.cpremise with
    | Hyp -> "H" ^ string_of_int c.name
    | Local -> "L" ^ string_of_int c.name
    | Lemma _ -> "T" ^ string_of_int c.name
    | History _ -> "C" ^ string_of_int c.name

  module Lit = struct
    type t = lit
    let[@inline] term l = l.term
    let[@inline] level l = l.l_level
    let[@inline] set_level l lvl = l.l_level <- lvl

    let[@inline] assigned l = l.assigned
    let[@inline] set_assigned l t = l.assigned <- t

    let[@inline] weight l = l.l_weight
    let[@inline] set_weight l w = l.l_weight <- w

    let make t =
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

    let debug_assign fmt v =
      match v.assigned with
      | None ->
        Format.fprintf fmt ""
      | Some t ->
        Format.fprintf fmt "@[<hov>@@%d->@ %a@]" v.l_level E.Term.print t

    let pp out v = E.Term.print out v.term
    let debug out v =
      Format.fprintf out "%d[%a][lit:@[<hov>%a@]]"
        (v.lid+1) debug_assign v E.Term.print v.term
  end

  module Var = struct
    type t = var
    let dummy = dummy_var
    let[@inline] level v = v.v_level
    let[@inline] set_level v lvl = v.v_level <- lvl
    let[@inline] pos v = v.pa
    let[@inline] neg v = v.na
    let[@inline] reason v = v.reason
    let[@inline] set_reason v r = v.reason <- r
    let[@inline] assignable v = v.v_assignable
    let[@inline] set_assignable v x = v.v_assignable <- x
    let[@inline] weight v = v.v_weight
    let[@inline] set_weight v w = v.v_weight <- w

    let make : formula -> var * Expr_intf.negated =
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

    (* Marking helpers *)
    let[@inline] clear v =
      v.v_fields <- Var_fields.empty

    let[@inline] seen_both v =
      Var_fields.get v_field_seen_pos v.v_fields &&
      Var_fields.get v_field_seen_neg v.v_fields
  end

  module Atom = struct
    type t = atom
    let dummy = dummy_atom
    let[@inline] level a = a.var.v_level
    let[@inline] var a = a.var
    let[@inline] neg a = a.neg
    let[@inline] abs a = a.var.pa
    let[@inline] lit a = a.lit
    let[@inline] equal a b = a == b
    let[@inline] compare a b = Pervasives.compare a.aid b.aid
    let[@inline] reason a = Var.reason a.var
    let[@inline] id a = a.aid
    let[@inline] is_true a = a.is_true
    let[@inline] is_false a = a.neg.is_true

    let[@inline] seen a =
      let pos = equal a (abs a) in
      if pos
      then Var_fields.get v_field_seen_pos a.var.v_fields
      else Var_fields.get v_field_seen_neg a.var.v_fields

    let[@inline] mark a =
      let pos = equal a (abs a) in
      if pos
      then a.var.v_fields <- Var_fields.set v_field_seen_pos true a.var.v_fields
      else a.var.v_fields <- Var_fields.set v_field_seen_neg true a.var.v_fields

    let[@inline] make lit =
      let var, negated = Var.make lit in
      match negated with
      | Formula_intf.Negated -> var.na
      | Formula_intf.Same_sign -> var.pa

    let pp fmt a = E.Formula.print fmt a.lit

    let pp_a fmt v =
      if Array.length v = 0 then (
        Format.fprintf fmt "∅"
      ) else (
        pp fmt v.(0);
        if (Array.length v) > 1 then begin
          for i = 1 to (Array.length v) - 1 do
            Format.fprintf fmt " ∨ %a" pp v.(i)
          done
        end
      )

    (* Complete debug printing *)
    let sign a = if a == a.var.pa then "+" else "-"

    let debug_reason fmt = function
      | n, _ when n < 0 ->
        Format.fprintf fmt "%%"
      | n, None ->
        Format.fprintf fmt "%d" n
      | n, Some Decision ->
        Format.fprintf fmt "@@%d" n
      | n, Some Bcp c ->
        Format.fprintf fmt "->%d/%s" n (name_of_clause c)
      | n, Some Semantic ->
        Format.fprintf fmt "::%d" n

    let pp_level fmt a =
      debug_reason fmt (a.var.v_level, a.var.reason)

    let debug_value fmt a =
      if a.is_true then
        Format.fprintf fmt "T%a" pp_level a
      else if a.neg.is_true then
        Format.fprintf fmt "F%a" pp_level a
      else
        Format.fprintf fmt ""

    let debug out a =
      Format.fprintf out "%s%d[%a][atom:@[<hov>%a@]]"
        (sign a) (a.var.vid+1) debug_value a E.Formula.print a.lit

    let debug_a out vec =
      Array.iter (fun a -> Format.fprintf out "%a@ " debug a) vec
  end

  (* Elements *)
  module Elt = struct
    type t = elt
    let[@inline] of_lit l = E_lit l
    let[@inline] of_var v = E_var v

    let[@inline] id = function
      | E_lit l -> l.lid | E_var v ->  v.vid
    let[@inline] level = function
      | E_lit l -> l.l_level | E_var v ->  v.v_level
    let[@inline] idx = function
      | E_lit l -> l.l_idx | E_var v ->  v.v_idx
    let[@inline] weight = function
      | E_lit l -> l.l_weight | E_var v ->  v.v_weight

    let[@inline] set_level e lvl = match e with
      | E_lit l -> l.l_level <- lvl | E_var v ->  v.v_level <- lvl
    let[@inline] set_idx e i = match e with
      | E_lit l -> l.l_idx <- i | E_var v ->  v.v_idx <- i
    let[@inline] set_weight e w = match e with
      | E_lit l -> l.l_weight <- w | E_var v ->  v.v_weight <- w
  end

  module Trail_elt = struct
    type t = trail_elt
    let[@inline] of_lit l = Lit l
    let[@inline] of_atom a = Atom a

    let debug fmt = function
      | Lit l -> Lit.debug fmt l
      | Atom a -> Atom.debug fmt a
  end

  module Clause = struct
    type t = clause
    let dummy = dummy_clause

    let make =
      let n = ref 0 in
      fun ?tag ali premise ->
        let atoms = Array.of_list ali in
        let name = !n in
        incr n;
        { name;
          tag = tag;
          atoms = atoms;
          visited = false;
          attached = false;
          activity = 0.;
          cpremise = premise}

    let empty = make [] (History [])
    let name = name_of_clause
    let[@inline] atoms c = c.atoms
    let[@inline] tag c = c.tag

    let[@inline] premise c = c.cpremise
    let[@inline] set_premise c p = c.cpremise <- p

    let[@inline] visited c = c.visited
    let[@inline] set_visited c b = c.visited <- b

    let[@inline] attached c = c.attached
    let[@inline] set_attached c b = c.attached <- b

    let[@inline] activity c = c.activity
    let[@inline] set_activity c w = c.activity <- w

    let pp fmt c =
      Format.fprintf fmt "%s : %a" (name c) Atom.pp_a c.atoms

    let debug_premise out = function
      | Hyp -> Format.fprintf out "hyp"
      | Local -> Format.fprintf out "local"
      | Lemma _ -> Format.fprintf out "th_lemma"
      | History v ->
        List.iter (fun c -> Format.fprintf out "%s,@ " (name_of_clause c)) v

    let debug out ({atoms=arr; cpremise=cp;_}as c) =
      Format.fprintf out "%s@[<hov>{@[<hov>%a@]}@ cpremise={@[<hov>%a@]}@]"
        (name c) Atom.debug_a arr debug_premise cp

    let pp_dimacs fmt {atoms;_} =
      let aux fmt a =
        Array.iter (fun p ->
          Format.fprintf fmt "%s%d "
            (if p == p.var.pa then "-" else "")
            (p.var.vid+1)
        ) a
      in
      Format.fprintf fmt "%a0" aux atoms
  end
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

