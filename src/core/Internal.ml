(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module type PLUGIN = sig
  val mcsat : bool
  (** Is this a mcsat plugin? *)

  val has_theory : bool
  (** Is this a CDCL(T) plugin or mcsat plugin?
      i.e does it have theories *)

  include Solver_intf.PLUGIN_MCSAT
end

let invalid_argf fmt =
  Format.kasprintf (fun msg -> invalid_arg ("msat: " ^ msg)) fmt

module Make(Plugin : PLUGIN)
= struct
  module Term = Plugin.Term
  module Formula = Plugin.Formula
  module Value = Plugin.Value

  type term = Term.t
  type formula = Formula.t
  type theory = Plugin.t
  type lemma = Plugin.proof
  type value = Value.t

  (* MCSAT literal *)
  type lit = {
    lid : int;
    term : term;
    mutable l_level : int;
    mutable l_idx: int;
    mutable l_weight : float;
    mutable assigned : value option;
  }

  type var = {
    vid : int;
    pa : atom;
    na : atom;
    mutable v_fields : int;
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
    watched : clause Vec.t;
  }

  and clause = {
    cid: int;
    atoms : atom array;
    mutable cpremise : premise;
    mutable activity : float;
    mutable flags: int; (* bitfield *)
  }

  and reason =
    | Decision
    | Bcp of clause
    | Bcp_lazy of clause lazy_t
    | Semantic

  (* TODO: remove, replace with user-provided proof trackng device?
     for pure SAT, [reason] is sufficient *)
  and premise =
    | Hyp of lemma
    | Local
    | Lemma of lemma
    | History of clause list
    | Empty_premise

  type elt =
    | E_lit of lit
    | E_var of var

  type trail_elt =
    | Lit of lit
    | Atom of atom

  (* Constructors *)
  module MF = Hashtbl.Make(Formula)
  module MT = Hashtbl.Make(Term)

  type st = {
    t_map: lit MT.t;
    f_map: var MF.t;
    vars: elt Vec.t;
    mutable cpt_mk_var: int;
    mutable cpt_mk_clause: int;
  }

  let create_st ?(size=`Big) () : st =
    let size_map = match size with
      | `Tiny -> 8
      | `Small -> 16
      | `Big -> 4096
    in
    { f_map = MF.create size_map;
      t_map = MT.create size_map;
      vars = Vec.create();
      cpt_mk_var = 0;
      cpt_mk_clause = 0;
    }

  let nb_elt st = Vec.size st.vars
  let get_elt st i = Vec.get st.vars i
  let iter_elt st f = Vec.iter f st.vars

  let name_of_clause c = match c.cpremise with
    | Hyp _ -> "H" ^ string_of_int c.cid
    | Lemma _ -> "T" ^ string_of_int c.cid
    | Local -> "L" ^ string_of_int c.cid
    | History _ -> "C" ^ string_of_int c.cid
    | Empty_premise -> string_of_int c.cid

  module Lit = struct
    type t = lit
    let[@inline] term l = l.term
    let[@inline] level l = l.l_level
    let[@inline] assigned l = l.assigned
    let[@inline] weight l = l.l_weight

    let make (st:st) (t:term) : t =
      try MT.find st.t_map t
      with Not_found ->
        let res = {
          lid = st.cpt_mk_var;
          term = t;
          l_weight = 1.;
          l_idx= -1;
          l_level = -1;
          assigned = None;
        } in
        st.cpt_mk_var <- st.cpt_mk_var + 1;
        MT.add st.t_map t res;
        Vec.push st.vars (E_lit res);
        res

    let debug_assign fmt v =
      match v.assigned with
      | None ->
        Format.fprintf fmt ""
      | Some t ->
        Format.fprintf fmt "@[<hov>@@%d->@ %a@]" v.l_level Value.pp t

    let pp out v = Term.pp out v.term
    let debug out v =
      Format.fprintf out "%d[%a][lit:@[<hov>%a@]]"
        (v.lid+1) debug_assign v Term.pp v.term
  end

  (* some boolean flags for variables, used as masks *)
  let seen_var = 0b1
  let seen_pos = 0b10
  let seen_neg = 0b100
  let default_pol_true = 0b1000

  module Var = struct
    type t = var
    let[@inline] level v = v.v_level
    let[@inline] pos v = v.pa
    let[@inline] neg v = v.na
    let[@inline] reason v = v.reason
    let[@inline] assignable v = v.v_assignable
    let[@inline] weight v = v.v_weight
    let[@inline] mark v = v.v_fields <- v.v_fields lor seen_var
    let[@inline] unmark v = v.v_fields <- v.v_fields land (lnot seen_var)
    let[@inline] marked v = (v.v_fields land seen_var) <> 0
    let[@inline] set_default_pol_true v = v.v_fields <- v.v_fields lor default_pol_true
    let[@inline] set_default_pol_false v = v.v_fields <- v.v_fields land (lnot default_pol_true)
    let[@inline] default_pol v = (v.v_fields land default_pol_true) <> 0

    let make ?(default_pol=true) (st:st) (t:formula) : var * Solver_intf.negated =
      let lit, negated = Formula.norm t in
      try
        MF.find st.f_map lit, negated
      with Not_found ->
        let cpt_double = st.cpt_mk_var lsl 1 in
        let rec var  =
          { vid = st.cpt_mk_var;
            pa = pa;
            na = na;
            v_fields = 0;
            v_level = -1;
            v_idx= -1;
            v_weight = 0.;
            v_assignable = None;
            reason = None;
          }
        and pa =
          { var = var;
            lit = lit;
            watched = Vec.create();
            neg = na;
            is_true = false;
            aid = cpt_double (* aid = vid*2 *) }
        and na =
          { var = var;
            lit = Formula.neg lit;
            watched = Vec.create();
            neg = pa;
            is_true = false;
            aid = cpt_double + 1 (* aid = vid*2+1 *) } in
        MF.add st.f_map lit var;
        st.cpt_mk_var <- st.cpt_mk_var + 1;
        if default_pol then set_default_pol_true var;
        Vec.push st.vars (E_var var);
        var, negated

    (* Marking helpers *)
    let[@inline] clear v =
      v.v_fields <- 0

    let[@inline] seen_both v =
      (seen_pos land v.v_fields <> 0) &&
      (seen_neg land v.v_fields <> 0)
  end

  module Atom = struct
    type t = atom
    let[@inline] level a = a.var.v_level
    let[@inline] var a = a.var
    let[@inline] neg a = a.neg
    let[@inline] abs a = a.var.pa
    let[@inline] formula a = a.lit
    let[@inline] equal a b = a == b
    let[@inline] sign a = a == abs a
    let[@inline] hash a = Hashtbl.hash a.aid
    let[@inline] compare a b = compare a.aid b.aid
    let[@inline] reason a = Var.reason a.var
    let[@inline] id a = a.aid
    let[@inline] is_true a = a.is_true
    let[@inline] is_false a = a.neg.is_true
    let has_value a = is_true a || is_false a

    let[@inline] seen a =
      if sign a
      then (seen_pos land a.var.v_fields <> 0)
      else (seen_neg land a.var.v_fields <> 0)

    let[@inline] mark a =
      let pos = equal a (abs a) in
      if pos then (
        a.var.v_fields <- seen_pos lor a.var.v_fields
      ) else (
        a.var.v_fields <- seen_neg lor a.var.v_fields
      )

    let[@inline] make ?default_pol st lit =
      let var, negated = Var.make ?default_pol st lit in
      match negated with
      | Solver_intf.Negated -> var.na
      | Solver_intf.Same_sign -> var.pa

    let pp fmt a = Formula.pp fmt a.lit

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
    let pp_sign a = if a == a.var.pa then "+" else "-"

    let debug_reason fmt = function
      | n, _ when n < 0 ->
        Format.fprintf fmt "%%"
      | n, None ->
        Format.fprintf fmt "%d" n
      | n, Some Decision ->
        Format.fprintf fmt "@@%d" n
      | n, Some Bcp c ->
        Format.fprintf fmt "->%d/%s" n (name_of_clause c)
      | n, Some (Bcp_lazy _) ->
        Format.fprintf fmt "->%d/<lazy>" n
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
        (pp_sign a) (a.var.vid+1) debug_value a Formula.pp a.lit

    let debug_a out vec =
      Array.iter (fun a -> Format.fprintf out "%a@ " debug a) vec
    let debug_l out l =
      List.iter (fun a -> Format.fprintf out "%a@ " debug a) l

    module Set = Set.Make(struct type t=atom let compare=compare end)
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

    let make_a =
      let n = ref 0 in
      fun ~flags atoms premise ->
        let cid = !n in
        incr n;
        { cid;
          atoms = atoms;
          flags;
          activity = 0.;
          cpremise = premise}

    let make ~flags l premise = make_a ~flags (Array.of_list l) premise

    let empty = make [] (History [])
    let name = name_of_clause
    let[@inline] equal c1 c2 = c1.cid = c2.cid
    let[@inline] hash c = Hashtbl.hash c.cid
    let[@inline] atoms c = c.atoms
    let[@inline] atoms_seq c = Iter.of_array c.atoms
    let[@inline] atoms_l c = Array.to_list c.atoms

    let flag_attached = 0b1
    let flag_visited = 0b10
    let flag_removable = 0b100
    let flag_dead = 0b1000

    let[@inline] make_removable l premise = make ~flags:flag_removable l premise
    let[@inline] make_removable_a l premise = make_a ~flags:flag_removable l premise
    let[@inline] make_permanent l premise = make ~flags:0 l premise

    let[@inline] visited c = (c.flags land flag_visited) <> 0
    let[@inline] set_visited c b =
      if b then c.flags <- c.flags lor flag_visited
      else c.flags <- c.flags land lnot flag_visited

    let[@inline] attached c = (c.flags land flag_attached) <> 0
    let[@inline] set_attached c b =
      if b then c.flags <- c.flags lor flag_attached
      else c.flags <- c.flags land lnot flag_attached

    let[@inline] removable c = (c.flags land flag_removable) <> 0
    let[@inline] set_removable c b =
      if b then c.flags <- c.flags lor flag_removable
      else c.flags <- c.flags land lnot flag_removable

    let[@inline] dead c = (c.flags land flag_dead) <> 0
    let[@inline] set_dead c = c.flags <- c.flags lor flag_dead

    let[@inline] activity c = c.activity
    let[@inline] set_activity c w = c.activity <- w

    module Tbl = Hashtbl.Make(struct
        type t = clause
        let hash = hash
        let equal = equal
      end)

    let pp fmt c =
      Format.fprintf fmt "%s : %a" (name c) Atom.pp_a c.atoms

    let debug_premise out = function
      | Hyp _ -> Format.fprintf out "hyp"
      | Lemma _ -> Format.fprintf out "th_lemma"
      | Local -> Format.fprintf out "local"
      | History v ->
        List.iter (fun c -> Format.fprintf out "%s,@ " (name_of_clause c)) v
      | Empty_premise -> Format.fprintf out "<no premise>"

    let debug out ({atoms=arr; cpremise=cp;_}as c) =
      Format.fprintf out "%s@[<hov>{@[<hov>%a@]}@ cpremise={@[<hov>%a@]}@]"
        (name c) Atom.debug_a arr debug_premise cp
  end

  module Proof =  struct
    exception Resolution_error of string

    type atom = Atom.t
    type clause = Clause.t
    type formula = Formula.t
    type lemma = Plugin.proof

    let error_res_f msg = Format.kasprintf (fun s -> raise (Resolution_error s)) msg

    let[@inline] clear_var_of_ (a:atom) = Var.clear a.var

    (* Compute resolution of 2 clauses.
       returns [pivots, resulting_atoms] *)
    let resolve (c1:clause) (c2:clause) : atom list * atom list =
      (* invariants: only atoms in [c2] are marked, and the pivot is
         cleared when traversing [c1] *)
      Array.iter Atom.mark c2.atoms;
      let pivots = ref [] in
      let l =
        Array.fold_left
          (fun l a ->
             if Atom.seen a then l
             else if Atom.seen a.neg then (
               pivots := a.var.pa :: !pivots;
               clear_var_of_ a;
               l
             ) else a::l)
          [] c1.atoms
      in
      let l =
        Array.fold_left (fun l a -> if Atom.seen a then a::l else l) l c2.atoms
      in
      Array.iter clear_var_of_ c2.atoms;
      !pivots, l

    (* [find_dups c] returns a list of duplicate atoms, and the deduplicated list *)
    let find_dups (c:clause) : atom list * atom list =
      let res =
        Array.fold_left
          (fun (dups,l) a ->
             if Atom.seen a then (
               a::dups, l
             ) else (
               Atom.mark a;
               dups, a::l
             ))
          ([], []) c.atoms
      in
      Array.iter clear_var_of_ c.atoms;
      res

    (* do [c1] and [c2] have the same lits, modulo reordering and duplicates? *)
    let same_lits (c1:atom Iter.t) (c2:atom Iter.t): bool =
      let subset a b =
        Iter.iter Atom.mark b;
        let res = Iter.for_all Atom.seen a in
        Iter.iter clear_var_of_ b;
        res
      in
      subset c1 c2 && subset c2 c1

    let prove conclusion =
      match conclusion.cpremise with
      | History [] -> assert false
      | Empty_premise -> raise Solver_intf.No_proof
      | _ -> conclusion

    let rec set_atom_proof a =
      let aux acc b =
        if Atom.equal a.neg b then acc
        else set_atom_proof b :: acc
      in
      assert (a.var.v_level >= 0);
      match (a.var.reason) with
      | Some (Bcp c | Bcp_lazy (lazy c)) ->
        Log.debugf 5 (fun k->k "(@[proof.analyze.clause@ :atom %a@ :c %a@])" Atom.debug a Clause.debug c);
        if Array.length c.atoms = 1 then (
          Log.debugf 5 (fun k -> k "(@[proof.analyze.old-reason@ %a@])" Atom.debug a);
          c
        ) else (
          assert (a.neg.is_true);
          let r = History (c :: (Array.fold_left aux [] c.atoms)) in
          let c' = Clause.make_permanent [a.neg] r in
          a.var.reason <- Some (Bcp c');
          Log.debugf 5
            (fun k -> k "(@[proof.analyze.new-reason@ :atom %a@ :c %a@])" Atom.debug a Clause.debug c');
          c'
        )
      | _ ->
        error_res_f "cannot prove atom %a" Atom.debug a

    let prove_unsat conflict =
      if Array.length conflict.atoms = 0 then (
        conflict
      ) else (
        Log.debugf 1 (fun k -> k "(@[sat.prove-unsat@ :from %a@])" Clause.debug conflict);
        let l = Array.fold_left (fun acc a -> set_atom_proof a :: acc) [] conflict.atoms in
        let res = Clause.make_permanent [] (History (conflict :: l)) in
        Log.debugf 1 (fun k -> k "(@[sat.proof-found@ %a@])" Clause.debug res);
        res
      )

    let prove_atom a =
      if a.is_true && a.var.v_level = 0 then
        Some (set_atom_proof a)
      else
        None

    type t = clause
    and proof_node = {
      conclusion : clause;
      step : step;
    }
    and step =
      | Hypothesis of lemma
      | Assumption
      | Lemma of lemma
      | Duplicate of t * atom list
      | Hyper_res of hyper_res_step

    and hyper_res_step = {
      hr_init: t;
      hr_steps: (atom * t) list; (* list of pivot+clause to resolve against [init] *)
    }

    let[@inline] conclusion (p:t) : clause = p

    type res_step = {
      rs_res: atom list;
      rs_c1: clause;
      rs_c2: clause;
      rs_pivot: atom;
    }

    (* find pivots for resolving [l] with [init], and also return
       the atoms of the conclusion *)
    let find_pivots (init:clause) (l:clause list) : _ * (atom * t) list =
      Log.debugf 15
        (fun k->k "(@[proof.find-pivots@ :init %a@ :l %a@])"
            Clause.debug init (Format.pp_print_list Clause.debug) l);
      Array.iter Atom.mark init.atoms;
      let steps =
        List.map
          (fun c ->
             let pivot =
               match
                 Iter.of_array c.atoms
                 |> Iter.filter (fun a -> Atom.seen (Atom.neg a))
                 |> Iter.to_list
               with
                 | [a] -> a
                 | [] ->
                   error_res_f "(@[proof.expand.pivot_missing@ %a@])" Clause.debug c
                 | pivots ->
                   error_res_f "(@[proof.expand.multiple_pivots@ %a@ :pivots %a@])"
                     Clause.debug c Atom.debug_l pivots
             in
             Array.iter Atom.mark c.atoms; (* add atoms to result *)
             clear_var_of_ pivot;
             Atom.abs pivot, c)
          l
      in
      (* cleanup *)
      let res = ref [] in
      let cleanup_a_ a =
        if Atom.seen a then (
          res := a :: !res;
          clear_var_of_ a
        )
      in
      Array.iter cleanup_a_ init.atoms;
      List.iter (fun c -> Array.iter cleanup_a_ c.atoms) l;
      !res, steps

    let expand conclusion =
      Log.debugf 5 (fun k -> k "(@[sat.proof.expand@ @[%a@]@])" Clause.debug conclusion);
      match conclusion.cpremise with
      | Lemma l ->
        { conclusion; step = Lemma l; }
      | Local ->
        { conclusion; step = Assumption; }
      | Hyp l ->
        { conclusion; step = Hypothesis l; }
      | History [] ->
        error_res_f "@[empty history for clause@ %a@]" Clause.debug conclusion
      | History [c] ->
        let duplicates, res = find_dups c in
        assert (same_lits (Iter.of_list res) (Clause.atoms_seq conclusion));
        { conclusion; step = Duplicate (c, duplicates) }
      | History (c :: r) ->
        let res, steps = find_pivots c r in
        assert (same_lits (Iter.of_list res) (Clause.atoms_seq conclusion));
        { conclusion; step = Hyper_res {hr_init=c; hr_steps=steps};  }
      | Empty_premise -> raise Solver_intf.No_proof

    let rec res_of_hyper_res (hr: hyper_res_step) : _ * _ * atom =
      let {hr_init=c1; hr_steps=l} = hr in
      match l with
      | [] -> assert false
      | [a, c2] -> c1, c2, a (* done *)
      | (a,c2) :: steps' ->
        (* resolve [c1] with [c2], then resolve that against [steps] *)
        let pivots, l = resolve c1 c2 in
        assert (match pivots with [a'] -> Atom.equal a a' | _ -> false);
        let c_1_2 = Clause.make_removable l (History [c1; c2]) in
        res_of_hyper_res {hr_init=c_1_2; hr_steps=steps'}

    (* Proof nodes manipulation *)
    let is_leaf = function
      | Hypothesis _
      | Assumption
      | Lemma _ -> true
      | Duplicate _
      | Hyper_res _ -> false

    let parents = function
      | Hypothesis _
      | Assumption
      | Lemma _ -> []
      | Duplicate (p, _) -> [p]
      | Hyper_res {hr_init; hr_steps} -> hr_init :: List.map snd hr_steps

    let expl = function
      | Hypothesis _ -> "hypothesis"
      | Assumption -> "assumption"
      | Lemma _ -> "lemma"
      | Duplicate _ -> "duplicate"
      | Hyper_res _ -> "hyper-resolution"

    (* Compute unsat-core by accumulating the leaves *)
    let unsat_core proof =
      let rec aux res acc = function
        | [] -> res, acc
        | c :: r ->
          if not @@ Clause.visited c then (
            Clause.set_visited c true;
            match c.cpremise with
            | Empty_premise -> raise Solver_intf.No_proof
            | Hyp _ | Lemma _ | Local -> aux (c :: res) acc r
            | History h ->
              let l = List.fold_left (fun acc c ->
                  if not @@ Clause.visited c then c :: acc else acc) r h in
              aux res (c :: acc) l
          ) else (
            aux res acc r
          )
      in
      let res, tmp = aux [] [] [proof] in
      List.iter (fun c -> Clause.set_visited c false) res;
      List.iter (fun c -> Clause.set_visited c false) tmp;
      res

    module Tbl = Clause.Tbl

    type task =
      | Enter of t
      | Leaving of t

    let spop s = try Some (Stack.pop s) with Stack.Empty -> None

    let rec fold_aux s h f acc =
      match spop s with
      | None -> acc
      | Some (Leaving c) ->
        Tbl.add h c true;
        fold_aux s h f (f acc (expand c))
      | Some (Enter c) ->
        if not (Tbl.mem h c) then begin
          Stack.push (Leaving c) s;
          let node = expand c in
          begin match node.step with
            | Duplicate (p1, _) ->
              Stack.push (Enter p1) s
            | Hyper_res {hr_init=p1; hr_steps=l} ->
              List.iter (fun (_,p2) -> Stack.push (Enter p2) s) l;
              Stack.push (Enter p1) s;
            | Hypothesis _ | Assumption | Lemma _ -> ()
          end
        end;
        fold_aux s h f acc

    let fold f acc p =
      let h = Tbl.create 42 in
      let s = Stack.create () in
      Stack.push (Enter p) s;
      fold_aux s h f acc

    let check_empty_conclusion (p:t) =
      if Array.length p.atoms > 0 then (
        error_res_f "@[<2>Proof.check: non empty conclusion for clause@ %a@]" Clause.debug p;
      )

    let check (p:t) = fold (fun () _ -> ()) () p
  end
  type proof = Proof.t

  module H = (Heap.Make [@specialise]) (struct
    type t = Elt.t
    let[@inline] cmp i j = Elt.weight j < Elt.weight i (* comparison by weight *)
    let idx = Elt.idx
    let set_idx = Elt.set_idx
  end)

  (* cause of "unsat", possibly conditional to local assumptions *)
  type unsat_cause =
    | US_local of {
        first: atom; (* assumption which was found to be proved false *)
        core: atom list; (* the set of assumptions *)
      }
    | US_false of clause (* true unsat *)

  exception E_sat
  exception E_unsat of unsat_cause
  exception UndecidedLit
  exception Restart
  exception Conflict of clause

  (* Log levels *)
  let error = 1
  let warn = 3
  let info = 5
  let debug = 50

  let var_decay : float = 1. /. 0.95
  (* inverse of the activity factor for variables. Default 1/0.95 *)

  let clause_decay : float = 1. /. 0.999
  (* inverse of the activity factor for clauses. Default 1/0.999 *)

  let restart_inc : float = 1.5
  (* multiplicative factor for restart limit, default 1.5 *)

  let learntsize_inc : float = 1.1
  (* multiplicative factor for [learntsize_factor] at each restart, default 1.1 *)

  (* Singleton type containing the current state *)
  type t = {
    st : st;
    th: theory;

    store_proof: bool; (* do we store proofs? *)

    (* Clauses are simplified for eficiency purposes. In the following
       vectors, the comments actually refer to the original non-simplified
       clause. *)

    clauses_hyps : clause Vec.t;
    (* clauses added by the user *)
    clauses_learnt : clause Vec.t;
    (* learnt clauses (tautologies true at any time, whatever the user level) *)

    clauses_to_add : clause Vec.t;
    (* Clauses either assumed or pushed by the theory, waiting to be added. *)

    mutable unsat_at_0: clause option;
    (* conflict at level 0, if any *)

    mutable next_decisions : atom list;
    (* When the last conflict was a semantic one (mcsat),
       this stores the next decision to make;
       if some theory wants atoms to be decided on (for theory combination),
       store them here. *)

    trail : trail_elt Vec.t;
    (* decision stack + propagated elements (atoms or assignments). *)

    elt_levels : int Vec.t;
    (* decision levels in [trail]  *)

    mutable assumptions: atom Vec.t;
    (* current assumptions *)

    mutable th_head : int;
    (* Start offset in the queue {!trail} of
       unit facts not yet seen by the theory. *)
    mutable elt_head : int;
    (* Start offset in the queue {!trail} of
       unit facts to propagate, within the trail *)

    (* invariant:
       - during propagation, th_head <= elt_head
       - then, once elt_head reaches length trail, Th.assume is
         called so that th_head can catch up with elt_head
       - this is repeated until a fixpoint is reached;
       - before a decision (and after the fixpoint),
         th_head = elt_head = length trail
    *)

    order : H.t;
    (* Heap ordered by variable activity *)

    to_clear: var Vec.t;
    (* variables to unmark *)

    mutable var_incr : float;
    (* increment for variables' activity *)

    mutable clause_incr : float;
    (* increment for clauses' activity *)

    mutable on_conflict : (atom array -> unit) option;
    mutable on_decision : (atom -> unit) option;
    mutable on_new_atom: (atom -> unit) option;
  }
  type solver = t

  (* intial restart limit *)
  let restart_first = 100

  (* initial limit for the number of learnt clauses, 1/3 of initial
      number of clauses by default *)
  let learntsize_factor = 1. /. 3.

  let _nop_on_conflict (_:atom array) = ()

  (* Starting environment. *)
  let create_ ~st ~store_proof (th:theory) : t = {
    st; th;
    unsat_at_0=None;
    next_decisions = [];

    clauses_hyps = Vec.create();
    clauses_learnt = Vec.create();

    clauses_to_add = Vec.create ();
    to_clear=Vec.create();

    th_head = 0;
    elt_head = 0;

    trail = Vec.create ();
    elt_levels = Vec.create();
    assumptions= Vec.create();

    order = H.create();

    var_incr = 1.;
    clause_incr = 1.;
    store_proof;
    on_conflict = None;
    on_decision= None;
    on_new_atom = None;
  }

  let create
      ?on_conflict ?on_decision ?on_new_atom
      ?(store_proof=true) ?(size=`Big) (th:theory) : t =
    let st = create_st ~size () in
    let st = create_ ~st ~store_proof th in
    st.on_new_atom <- on_new_atom;
    st.on_decision <- on_decision;
    st.on_conflict <- on_conflict;
    st

  let[@inline] st t = t.st
  let[@inline] nb_clauses st = Vec.size st.clauses_hyps
  let[@inline] decision_level st = Vec.size st.elt_levels

  (* Do we have a level-0 empty clause? *)
  let[@inline] check_unsat_ st =
    match st.unsat_at_0 with
    | Some c -> raise (E_unsat (US_false c))
    | None -> ()

  (* Iteration over subterms.
     When incrementing activity, we want to be able to iterate over
     all subterms of a formula. However, the function provided by the theory
     may be costly (if it walks a tree-like structure, and does some processing
     to ignore some subterms for instance), so we want to 'cache' the list
     of subterms of each formula, so we have a field [v_assignable]
     directly in variables to do so.  *)
  let iter_sub f v =
    if Plugin.mcsat then (
      match v.v_assignable with
      | Some l -> List.iter f l
      | None -> assert false
    )

  let mk_atom_mcsat_ st a =
    match a.var.v_assignable with
    | Some _ -> ()
    | None ->
      let l = ref [] in
      Plugin.iter_assignable st.th
        (fun t -> l := Lit.make st.st t :: !l)
        a.var.pa.lit;
      a.var.v_assignable <- Some !l;
      ()

  (* When we have a new literal,
     we need to first create the list of its subterms. *)
  let mk_atom ?default_pol st (f:formula) : atom =
    let res = Atom.make ?default_pol st.st f in
    if Plugin.mcsat then (
      mk_atom_mcsat_ st res;
    );
    res

  (* Variable and literal activity.
     Activity is used to decide on which variable to decide when propagation
     is done. Uses a heap (implemented in Iheap), to keep track of variable activity.
     To be more general, the heap only stores the variable/literal id (i.e an int).
     When we add a variable (which wraps a formula), we also need to add all
     its subterms.
  *)
  let rec insert_elt_order st (elt:elt) : unit =
    H.insert st.order elt;
    if Plugin.mcsat then (
      match elt with
      | E_lit _ -> ()
      | E_var v -> insert_subterms_order st v
    )

  and insert_var_order st (v:var) : unit =
    insert_elt_order st (E_var v)

  and insert_subterms_order st (v:var) : unit =
    iter_sub (fun t -> insert_elt_order st (Elt.of_lit t)) v

  (* Add new litterals/atoms on which to decide on, even if there is no
     clause that constrains it.
     We could maybe check if they have already has been decided before
     inserting them into the heap, if it appears that it helps performance. *)
  let make_term st t =
    let l = Lit.make st.st t in
    if l.l_level < 0 then (
      insert_elt_order st (E_lit l);
    )

  let make_atom st (p:formula) : atom =
    let a = mk_atom st p in
    if a.var.v_level < 0 then (
      insert_elt_order st (E_var a.var);
      (match st.on_new_atom with Some f -> f a | None -> ());
    ) else (
      assert (a.is_true || a.neg.is_true);
    );
    a

  (* Rather than iterate over all the heap when we want to decrease all the
     variables/literals activity, we instead increase the value by which
     we increase the activity of 'interesting' var/lits. *)
  let[@inline] var_decay_activity st =
    st.var_incr <- st.var_incr *. var_decay

  let[@inline] clause_decay_activity st =
    st.clause_incr <- st.clause_incr *. clause_decay

  (* increase activity of [v] *)
  let var_bump_activity_aux st v =
    v.v_weight <- v.v_weight +. st.var_incr;
    if v.v_weight > 1e100 then (
      for i = 0 to nb_elt st.st - 1 do
        Elt.set_weight (get_elt st.st i) ((Elt.weight (get_elt st.st i)) *. 1e-100)
      done;
      st.var_incr <- st.var_incr *. 1e-100;
    );
    let elt = Elt.of_var v in
    if H.in_heap elt then (
      H.decrease st.order elt
    )

  (* increase activity of literal [l] *)
  let lit_bump_activity_aux (st:t) (l:lit): unit =
    l.l_weight <- l.l_weight +. st.var_incr;
    if l.l_weight > 1e100 then (
      iter_elt st.st (fun e -> Elt.set_weight e (Elt.weight e *. 1e-100));
      st.var_incr <- st.var_incr *. 1e-100;
    );
    let elt = Elt.of_lit l in
    if H.in_heap elt then (
      H.decrease st.order elt
    )

  (* increase activity of var [v] *)
  let var_bump_activity st (v:var): unit =
    var_bump_activity_aux st v;
    iter_sub (lit_bump_activity_aux st) v

  (* increase activity of clause [c] *)
  let clause_bump_activity st (c:clause) : unit =
    c.activity <- c.activity +. st.clause_incr;
    if c.activity > 1e20 then (
      Vec.iter (fun c -> c.activity <- c.activity *. 1e-20) st.clauses_learnt;
      st.clause_incr <- st.clause_incr *. 1e-20
    )

  (* Simplification of clauses.

     When adding new clauses, it is desirable to 'simplify' them, i.e
     minimize the amount of literals in it, because it greatly reduces
     the search space for new watched literals during propagation.
     Additionally, we have to partition the lits, to ensure the watched
     literals (which are the first two lits of the clause) are appropriate.
     Indeed, it is better to watch true literals, and then unassigned literals.
     Watching false literals should be a last resort, and come with constraints
     (see {!add_clause}).
  *)
  exception Trivial

  (* [arr_to_list a i] converts [a.(i), ... a.(length a-1)] into a list *)
  let arr_to_list arr i : _ list =
    if i >= Array.length arr then []
    else Array.to_list (Array.sub arr i (Array.length arr - i))

  (* Eliminates atom duplicates in clauses *)
  let eliminate_duplicates clause : clause =
    let trivial = ref false in
    let duplicates = ref [] in
    let res = ref [] in
    Array.iter (fun a ->
        if Atom.seen a then duplicates := a :: !duplicates
        else (
          Atom.mark a;
          res := a :: !res
        ))
      clause.atoms;
    List.iter
      (fun a ->
         if Var.seen_both a.var then trivial := true;
         Var.clear a.var)
      !res;
    if !trivial then (
      raise Trivial
    ) else if !duplicates = [] then (
      clause
    ) else (
      Clause.make ~flags:clause.flags !res (History [clause])
    )

  (* Partition literals for new clauses, into:
     - true literals (maybe makes the clause trivial if the lit is proved true at level 0)
     - unassigned literals, yet to be decided
     - false literals (not suitable to watch, those at level 0 can be removed from the clause)

     Clauses that propagated false lits are remembered to reconstruct resolution proofs.
  *)
  let partition atoms : atom list * clause list =
    let rec partition_aux trues unassigned falses history i =
      if i >= Array.length atoms then (
        trues @ unassigned @ falses, history
      ) else (
        let a = atoms.(i) in
        if a.is_true then (
          let l = a.var.v_level in
          if l = 0 then
            raise Trivial (* A var true at level 0 gives a trivially true clause *)
          else
            (a :: trues) @ unassigned @ falses @
            (arr_to_list atoms (i + 1)), history
        ) else if a.neg.is_true then (
          let l = a.var.v_level in
          if l = 0 then (
            match a.var.reason with
            | Some (Bcp cl | Bcp_lazy (lazy cl)) ->
              partition_aux trues unassigned falses (cl :: history) (i + 1)
            (* A var false at level 0 can be eliminated from the clause,
               but we need to kepp in mind that we used another clause to simplify it. *)
            | Some Semantic ->
              partition_aux trues unassigned falses history (i + 1)
            (* Semantic propagations at level 0 are, well not easy to deal with,
               this shouldn't really happen actually (because semantic propagations
               at level 0 should come with a proof). *)
            (* TODO: get a proof of the propagation. *)
            | None | Some Decision -> assert false
            (* The var must have a reason, and it cannot be a decision/assumption,
               since its level is 0. *)
          ) else (
            partition_aux trues unassigned (a::falses) history (i + 1)
          )
        ) else (
          partition_aux trues (a::unassigned) falses history (i + 1)
        )
      )
    in
    partition_aux [] [] [] [] 0


  (* Making a decision.
     Before actually creatig a new decision level, we check that
     all propagations have been done and propagated to the theory,
     i.e that the theoriy state indeed takes into account the whole
     stack of literals
     i.e we have indeed reached a propagation fixpoint before making
     a new decision *)
  let new_decision_level st =
    assert (st.th_head = Vec.size st.trail);
    assert (st.elt_head = Vec.size st.trail);
    Vec.push st.elt_levels (Vec.size st.trail);
    Plugin.push_level st.th;
    ()

  (* Attach/Detach a clause.

     A clause is attached (to its watching lits) when it is first added,
     either because it is assumed or learnt.

  *)
  let attach_clause c =
    assert (not @@ Clause.attached c);
    Log.debugf debug (fun k -> k "(@[sat.attach-clause@ %a@])" Clause.debug c);
    Vec.push c.atoms.(0).neg.watched c;
    Vec.push c.atoms.(1).neg.watched c;
    Clause.set_attached c true;
    ()

  (* Backtracking.
     Used to backtrack, i.e cancel down to [lvl] excluded,
     i.e we want to go back to the state the solver was in
         when decision level [lvl] was created. *)
  let cancel_until st lvl =
    assert (lvl >= 0);
    (* Nothing to do if we try to backtrack to a non-existent level. *)
    if decision_level st <= lvl then (
      Log.debugf debug (fun k -> k "(@[sat.cancel-until.nop@ :already-at-level <= %d@])" lvl)
    ) else (
      Log.debugf info (fun k -> k "(@[sat.cancel-until %d@])" lvl);
      (* We set the head of the solver and theory queue to what it was. *)
      let head = ref (Vec.get st.elt_levels lvl) in
      st.elt_head <- !head;
      st.th_head <- !head;
      (* Now we need to cleanup the vars that are not valid anymore
         (i.e to the right of elt_head in the queue. *)
      for c = st.elt_head to Vec.size st.trail - 1 do
        match (Vec.get st.trail c) with
        (* A literal is unassigned, we nedd to add it back to
           the heap of potentially assignable literals, unless it has
           a level lower than [lvl], in which case we just move it back. *)
        | Lit l ->
          if l.l_level <= lvl then (
            Vec.set st.trail !head (Trail_elt.of_lit l);
            head := !head + 1
          ) else (
            l.assigned <- None;
            l.l_level <- -1;
            insert_elt_order st (Elt.of_lit l)
          )
        (* A variable is not true/false anymore, one of two things can happen: *)
        | Atom a ->
          if a.var.v_level <= lvl then (
            (* It is a late propagation, which has a level
               lower than where we backtrack, so we just move it to the head
               of the queue, to be propagated again. *)
            Vec.set st.trail !head (Trail_elt.of_atom a);
            head := !head + 1
          ) else (
            (* it is a result of bolean propagation, or a semantic propagation
               with a level higher than the level to which we backtrack,
               in that case, we simply unset its value and reinsert it into the heap. *)
            a.is_true <- false;
            a.neg.is_true <- false;
            a.var.v_level <- -1;
            a.var.reason <- None;
            insert_elt_order st (Elt.of_var a.var)
          )
      done;
      (* Recover the right theory state. *)
      let n = decision_level st - lvl in
      assert (n>0);
      (* Resize the vectors according to their new size. *)
      Vec.shrink st.trail !head;
      Vec.shrink st.elt_levels lvl;
      Plugin.pop_levels st.th n;
      st.next_decisions <- [];
    );
    ()

  let pp_unsat_cause out = function
    | US_local {first=_; core} ->
      Format.fprintf out "(@[unsat-cause@ :false-assumptions %a@])"
        (Format.pp_print_list Atom.pp) core
    | US_false c ->
      Format.fprintf out "(@[unsat-cause@ :false %a@])" Clause.debug c

  (* Unsatisfiability is signaled through an exception, since it can happen
     in multiple places (adding new clauses, or solving for instance). *)
  let report_unsat st (us:unsat_cause) : _ =
    Log.debugf info (fun k -> k "(@[sat.unsat-conflict@ %a@])" pp_unsat_cause us);
    let us = match us with
      | US_false c ->
        let c = if st.store_proof then Proof.prove_unsat c else c in
        st.unsat_at_0 <- Some c;
        US_false c
      | _ -> us
    in
    raise (E_unsat us)

  (* Simplification of boolean propagation reasons.
     When doing boolean propagation *at level 0*, it can happen
     that the clause cl, which propagates a formula, also contains
     other formulas, but has been simplified. in which case, we
     need to rebuild a clause with correct history, in order to
     be able to build a correct proof at the end of proof search. *)
  let simpl_reason : reason -> reason = function
    | (Bcp cl | Bcp_lazy (lazy cl)) as r ->
      let l, history = partition cl.atoms in
      begin match l with
        | [_] ->
          if history = [] then (
            (* no simplification has been done, so [cl] is actually a clause with only
               [a], so it is a valid reason for propagating [a]. *)
            r
          ) else (
            (* Clauses in [history] have been used to simplify [cl] into a clause [tmp_cl]
               with only one formula (which is [a]). So we explicitly create that clause
               and set it as the cause for the propagation of [a], that way we can
               rebuild the whole resolution tree when we want to prove [a]. *)
            let c' = Clause.make ~flags:cl.flags l (History (cl :: history)) in
            Log.debugf debug
              (fun k -> k "(@[<hv>sat.simplified-reason@ %a@ %a@])" Clause.debug cl Clause.debug c');
            Bcp c'
          )
        | _ ->
          Log.debugf error
            (fun k ->
               k "(@[<v2>sat.simplify-reason.failed@ :at %a@ %a@]"
                 (Vec.pp ~sep:"" Atom.debug) (Vec.of_list l)
                 Clause.debug cl);
          assert false
      end
    | (Decision | Semantic) as r -> r

  (* Boolean propagation.
     Wrapper function for adding a new propagated formula. *)
  let enqueue_bool st a ~level:lvl reason : unit =
    if a.neg.is_true then (
      Log.debugf error
        (fun k->k "(@[sat.error.trying to enqueue a false literal %a@])" Atom.debug a);
      assert false
    );
    assert (not a.is_true && a.var.v_level < 0 &&
            a.var.reason = None && lvl >= 0);
    let reason =
      if lvl > 0 then reason
      else simpl_reason reason
    in
    a.is_true <- true;
    a.var.v_level <- lvl;
    a.var.reason <- Some reason;
    Vec.push st.trail (Trail_elt.of_atom a);
    Log.debugf debug
      (fun k->k "(@[sat.enqueue[%d]@ %a@])" (Vec.size st.trail) Atom.debug a);
    ()

  let enqueue_semantic st a terms =
    if not a.is_true then (
      let l = List.map (Lit.make st.st) terms in
      let lvl = List.fold_left (fun acc {l_level; _} ->
          assert (l_level > 0); max acc l_level) 0 l in
      enqueue_bool st a ~level:lvl Semantic
    )

  (* MCsat semantic assignment *)
  let enqueue_assign st (l:lit) (value:value) lvl =
    match l.assigned with
    | Some _ ->
      Log.debugf error
        (fun k -> k "(@[sat.error: Trying to assign an already assigned literal:@ %a@])" Lit.debug l);
      assert false
    | None ->
      assert (l.l_level < 0);
      l.assigned <- Some value;
      l.l_level <- lvl;
      Vec.push st.trail (Trail_elt.of_lit l);
      Log.debugf debug
        (fun k -> k "(@[sat.enqueue-semantic[%d]@ %a@])" (Vec.size st.trail) Lit.debug l);
      ()

  (* swap elements of array *)
  let[@inline] swap_arr a i j =
    if i<>j then (
      let tmp = a.(i) in
      a.(i) <- a.(j);
      a.(j) <- tmp;
    )

  (* move atoms assigned at high levels first *)
  let put_high_level_atoms_first (arr:atom array) : unit =
    Array.iteri
      (fun i a ->
         if i>0 && Atom.level a > Atom.level arr.(0) then (
           (* move first to second, [i]-th to first, second to [i] *)
           if i=1 then (
             swap_arr arr 0 1;
           ) else (
             let tmp = arr.(1) in
             arr.(1) <- arr.(0);
             arr.(0) <- arr.(i);
             arr.(i) <- tmp;
           );
         ) else if i>1 && Atom.level a > Atom.level arr.(1) then (
           swap_arr arr 1 i;
         ))
      arr

  (* evaluate an atom for MCsat, if it's not assigned
     by boolean propagation/decision *)
  let th_eval st a : bool option =
    if a.is_true || a.neg.is_true then None
    else match Plugin.eval st.th a.lit with
      | Solver_intf.Unknown -> None
      | Solver_intf.Valued (b, l) ->
        if l = [] then (
          invalid_argf "semantic propagation at level 0 currently forbidden: %a" Atom.pp a;
        );
        let atom = if b then a else a.neg in
        enqueue_semantic st atom l;
        Some b

  (* find which level to backtrack to, given a conflict clause
     and a boolean stating whether it is
     a UIP ("Unique Implication Point")
     precond: the atom list is sorted by decreasing decision level *)
  let backtrack_lvl _st (arr: atom array) : int * bool =
    if Array.length arr <= 1 then (
      0, true
    ) else (
      let a = arr.(0) in
      let b = arr.(1) in
      assert(a.var.v_level > 0);
      if a.var.v_level > b.var.v_level then (
        (* backtrack below [a], so we can propagate [not a] *)
        b.var.v_level, true
      ) else (
        assert (a.var.v_level = b.var.v_level);
        assert (a.var.v_level >= 0);
        max (a.var.v_level - 1) 0, false
      )
    )

  (* result of conflict analysis, containing the learnt clause and some
     additional info.

     invariant: cr_history's order matters, as its head is later used
     during pop operations to determine the origin of a clause/conflict
     (boolean conflict i.e hypothesis, or theory lemma) *)
  type conflict_res = {
    cr_backtrack_lvl : int; (* level to backtrack to *)
    cr_learnt: atom array; (* lemma learnt from conflict *)
    cr_history: clause list; (* justification *)
    cr_is_uip: bool; (* conflict is UIP? *)
  }

  let[@inline] get_atom st i =
    match Vec.get st.trail i with
    | Atom x -> x
    | Lit _ -> assert false

  (* conflict analysis for SAT
     Same idea as the mcsat analyze function (without semantic propagations),
     except we look the the Last UIP (TODO: check ?), and do it in an imperative
     and efficient manner. *)
  let analyze st c_clause : conflict_res =
    let pathC  = ref 0 in
    let learnt = ref [] in
    let cond   = ref true in
    let blevel = ref 0 in
    let to_unmark = st.to_clear in (* for cleanup *)
    let c      = ref (Some c_clause) in
    let tr_ind = ref (Vec.size st.trail - 1) in
    let history = ref [] in
    assert (decision_level st > 0);
    Vec.clear to_unmark;
    let conflict_level =
      if Plugin.mcsat || Plugin.has_theory
      then Array.fold_left (fun acc p -> max acc p.var.v_level) 0 c_clause.atoms
      else decision_level st
    in
    Log.debugf debug
      (fun k -> k "(@[sat.analyze-conflict@ :c-level %d@ :clause %a@])" conflict_level Clause.debug c_clause);
    while !cond do
      begin match !c with
        | None ->
          Log.debug debug "(@[sat.analyze-conflict: skipping resolution for semantic propagation@])"
        | Some clause ->
          Log.debugf debug (fun k->k"(@[sat.analyze-conflict.resolve@ %a@])"  Clause.debug clause);
          if Clause.removable clause then (
            clause_bump_activity st clause;
          );
          history := clause :: !history;
          (* visit the current predecessors *)
          for j = 0 to Array.length clause.atoms - 1 do
            let q = clause.atoms.(j) in
            assert (q.is_true || q.neg.is_true && q.var.v_level >= 0); (* unsure? *)
            if q.var.v_level <= 0 then (
              assert (q.neg.is_true);
              match q.var.reason with
              | Some (Bcp cl | Bcp_lazy (lazy cl)) -> history := cl :: !history
              | Some (Decision | Semantic) | None -> assert false
            );
            if not (Var.marked q.var) then (
              Var.mark q.var;
              Vec.push to_unmark q.var;
              if q.var.v_level > 0 then (
                var_bump_activity st q.var;
                if q.var.v_level >= conflict_level then (
                  incr pathC;
                ) else (
                  learnt := q :: !learnt;
                  blevel := max !blevel q.var.v_level
                )
              )
            )
          done
      end;

      (* look for the next node to expand *)
      while
        let a = Vec.get st.trail !tr_ind in
        Log.debugf debug
          (fun k -> k "(@[sat.analyze-conflict.at-trail-elt@ %a@])" Trail_elt.debug a);
        match a with
        | Atom q ->
          (not (Var.marked q.var)) ||
          (q.var.v_level < conflict_level)
        | Lit _ -> true
      do
        decr tr_ind;
      done;
      let p = get_atom st !tr_ind in
      decr pathC;
      decr tr_ind;
      match !pathC, p.var.reason with
      | 0, _ ->
        cond := false;
        learnt := p.neg :: List.rev !learnt
      | n, Some Semantic ->
        assert (n > 0);
        learnt := p.neg :: !learnt;
        c := None
      | n, Some (Bcp cl | Bcp_lazy (lazy cl)) ->
        assert (n > 0);
        assert (p.var.v_level >= conflict_level);
        c := Some cl
      | _, (None | Some Decision) -> assert false
    done;
    Vec.iter Var.clear to_unmark;
    Vec.clear to_unmark;
    (* put high-level literals first, so that:
       - they make adequate watch lits
       - the first literal is the UIP, if any *)
    let a = Array.of_list !learnt in
    Array.fast_sort (fun p q -> compare q.var.v_level p.var.v_level) a;
    (* put_high_level_atoms_first a; *)
    let level, is_uip = backtrack_lvl st a in
    { cr_backtrack_lvl = level;
      cr_learnt = a;
      cr_history = List.rev !history;
      cr_is_uip = is_uip;
    }

  (* add the learnt clause to the clause database, propagate, etc. *)
  let record_learnt_clause st (confl:clause) (cr:conflict_res): unit =
    let proof = if st.store_proof then History cr.cr_history else Empty_premise in
    begin match cr.cr_learnt with
      | [| |] -> assert false
      | [|fuip|] ->
        assert (cr.cr_backtrack_lvl = 0 && decision_level st = 0);
        if fuip.neg.is_true then (
          (* incompatible at level 0 *)
          report_unsat st (US_false confl)
        ) else (
          let uclause = Clause.make_removable_a cr.cr_learnt proof  in
          (* no need to attach [uclause], it is true at level 0 *)
          enqueue_bool st fuip ~level:0 (Bcp uclause)
        )
      | _ ->
        let fuip = cr.cr_learnt.(0) in
        let lclause = Clause.make_removable_a cr.cr_learnt proof in
        if Array.length lclause.atoms > 2 then (
          Vec.push st.clauses_learnt lclause; (* potentially gc'able *)
        );
        attach_clause lclause;
        clause_bump_activity st lclause;
        if cr.cr_is_uip then (
          enqueue_bool st fuip ~level:cr.cr_backtrack_lvl (Bcp lclause)
        ) else (
          assert Plugin.mcsat;
          assert (st.next_decisions = []);
          st.next_decisions <- [fuip.neg];
        )
    end;
    var_decay_activity st;
    clause_decay_activity st

  (* process a conflict:
     - learn clause
     - backtrack
     - report unsat if conflict at level 0
  *)
  let add_boolean_conflict st (confl:clause): unit =
    Log.debugf info (fun k -> k "(@[sat.add-bool-conflict@ %a@])" Clause.debug confl);
    st.next_decisions <- [];
    assert (decision_level st >= 0);
    if decision_level st = 0 ||
       Array.for_all (fun a -> a.var.v_level <= 0) confl.atoms then (
      (* Top-level conflict *)
      report_unsat st (US_false confl);
    );
    let cr = analyze st confl in
    cancel_until st (max cr.cr_backtrack_lvl 0);
    record_learnt_clause st confl cr

  (* Get the correct vector to insert a clause in. *)
  let[@inline] add_clause_to_vec st c =
    if Clause.removable c then (
      Vec.push st.clauses_learnt c
    ) else (
      Vec.push st.clauses_hyps c
    )

  (* Add a new clause, simplifying, propagating, and backtracking if
     the clause is false in the current trail *)
  let add_clause_ st (init:clause) : unit =
    Log.debugf debug (fun k -> k "(@[sat.add-clause@ @[<hov>%a@]@])" Clause.debug init);
    (* Insertion of new lits is done before simplification. Indeed, else a lit in a
       trivial clause could end up being not decided on, which is a bug. *)
    Array.iter (fun x -> insert_elt_order st (Elt.of_var x.var)) init.atoms;
    try
      let c = eliminate_duplicates init in
      assert (c.flags = init.flags);
      Log.debugf debug (fun k -> k "(@[sat.dups-removed@ %a@])" Clause.debug c);
      let atoms, history = partition c.atoms in
      let clause =
        if history = [] then (
          (* just update order of atoms *)
          List.iteri (fun i a -> c.atoms.(i) <- a) atoms;
          c
        ) else (
          let proof = if st.store_proof then History (c::history) else Empty_premise in
          Clause.make ~flags:c.flags atoms proof
        )
      in
      assert (clause.flags = init.flags);
      Log.debugf info (fun k->k "(@[sat.new-clause@ @[<hov>%a@]@])" Clause.debug clause);
      match atoms with
      | [] ->
        report_unsat st @@ US_false clause
      | [a]   ->
        cancel_until st 0;
        if a.neg.is_true then (
          (* cannot recover from this *)
          report_unsat st @@ US_false clause
        ) else if a.is_true then (
          () (* atom is already true, nothing to do *)
        ) else (
          Log.debugf debug
            (fun k->k "(@[sat.add-clause.unit-clause@ :propagating %a@])" Atom.debug a);
          add_clause_to_vec st clause;
          enqueue_bool st a ~level:0 (Bcp clause)
        )
      | a::b::_ ->
        add_clause_to_vec st clause;
        if a.neg.is_true then (
          (* Atoms need to be sorted in decreasing order of decision level,
             or we might watch the wrong literals. *)
          put_high_level_atoms_first clause.atoms;
          attach_clause clause;
          add_boolean_conflict st clause
        ) else (
          attach_clause clause;
          if b.neg.is_true && not a.is_true && not a.neg.is_true then (
            let lvl = List.fold_left (fun m a -> max m a.var.v_level) 0 atoms in
            cancel_until st lvl;
            enqueue_bool st a ~level:lvl (Bcp clause)
          )
        )
    with Trivial ->
      Log.debugf info
        (fun k->k "(@[sat.add-clause@ :ignore-trivial @[%a@]@])" Clause.debug init)

  let[@inline never] flush_clauses_ st =
    while not @@ Vec.is_empty st.clauses_to_add do
      let c = Vec.pop st.clauses_to_add in
      add_clause_ st c
    done

  let[@inline] flush_clauses st =
    if not @@ Vec.is_empty st.clauses_to_add then flush_clauses_ st

  type watch_res =
    | Watch_kept
    | Watch_removed

  (* boolean propagation.
     [a] is the false atom, one of [c]'s two watch literals
     [i] is the index of [c] in [a.watched]
     @return whether [c] was removed from [a.watched]
  *)
  let propagate_in_clause st (a:atom) (c:clause) (i:int): watch_res =
    let atoms = c.atoms in
    let first = atoms.(0) in
    if first == a.neg then (
      (* false lit must be at index 1 *)
      atoms.(0) <- atoms.(1);
      atoms.(1) <- first
    ) else (
      assert (a.neg == atoms.(1))
    );
    let first = atoms.(0) in
    if first.is_true
    then Watch_kept (* true clause, keep it in watched *)
    else (
      try (* look for another watch lit *)
        for k = 2 to Array.length atoms - 1 do
          let ak = atoms.(k) in
          if not (ak.neg.is_true) then (
            (* watch lit found: update and exit *)
            atoms.(1) <- ak;
            atoms.(k) <- a.neg;
            (* remove [c] from [a.watched], add it to [ak.neg.watched] *)
            Vec.push ak.neg.watched c;
            assert (Vec.get a.watched i == c);
            Vec.fast_remove a.watched i;
            raise_notrace Exit
          )
        done;
        (* no watch lit found *)
        if first.neg.is_true then (
          (* clause is false *)
          st.elt_head <- Vec.size st.trail;
          raise_notrace (Conflict c)
        ) else (
          match th_eval st first with
          | None -> (* clause is unit, keep the same watches, but propagate *)
            enqueue_bool st first ~level:(decision_level st) (Bcp c)
          | Some true -> ()
          | Some false ->
            st.elt_head <- Vec.size st.trail;
            raise_notrace (Conflict c)
        );
        Watch_kept
      with Exit ->
        Watch_removed
    )

  (* propagate atom [a], which was just decided. This checks every
     clause watching [a] to see if the clause is false, unit, or has
     other possible watches
     @param res the optional conflict clause that the propagation might trigger *)
  let propagate_atom st a : unit =
    let watched = a.watched in
    let rec aux i =
      if i >= Vec.size watched then ()
      else (
        let c = Vec.get watched i in
        assert (Clause.attached c);
        let j =
          if Clause.dead c then (
            Vec.fast_remove watched i;
            i
          ) else (
            match propagate_in_clause st a c i with
            | Watch_kept -> i+1
            | Watch_removed -> i (* clause at this index changed *)
          )
        in
        aux j
      )
    in
    aux 0

  (* Propagation (boolean and theory) *)
  let create_atom ?default_pol st f =
    let a = mk_atom ?default_pol st f in
    ignore (th_eval st a);
    a

  exception Th_conflict of Clause.t

  let slice_get st i =
    match Vec.get st.trail i with
    | Atom a ->
      Solver_intf.Lit a.lit
    | Lit {term; assigned = Some v; _} ->
      Solver_intf.Assign (term, v)
    | Lit _ -> assert false

  let acts_add_clause st ?(keep=false) (l:formula list) (lemma:lemma): unit =
    let atoms = List.rev_map (create_atom st) l in
    let flags = if keep then 0 else Clause.flag_removable in
    let c = Clause.make ~flags atoms (Lemma lemma) in
    Log.debugf info (fun k->k "(@[sat.th.add-clause@ %a@])" Clause.debug c);
    Vec.push st.clauses_to_add c

  let acts_add_decision_lit (st:t) (f:formula) (sign:bool) : unit =
    let a = create_atom st f in
    let a = if sign then a else Atom.neg a in
    if not (Atom.has_value a) then (
      Log.debugf 10 (fun k->k "(@[sat.th.add-decision-lit@ %a@])" Atom.debug a);
      st.next_decisions <- a :: st.next_decisions
    )

  let acts_raise st (l:formula list) proof : 'a =
    let atoms = List.rev_map (create_atom st) l in
    (* conflicts can be removed *)
    let c = Clause.make_removable atoms (Lemma proof) in
    Log.debugf 5 (fun k->k "(@[@{<yellow>sat.th.raise-conflict@}@ %a@])" Clause.debug c);
    raise_notrace (Th_conflict c)

  let check_consequence_lits_false_ l : unit =
    match List.find Atom.is_true l with
    | a ->
      invalid_argf
        "slice.acts_propagate:@ Consequence should contain only true literals, but %a isn't"
        Atom.debug (Atom.neg a)
    | exception Not_found -> ()

  let acts_propagate (st:t) f = function
    | Solver_intf.Eval l ->
      let a = mk_atom st f in
      enqueue_semantic st a l
    | Solver_intf.Consequence mk_expl ->
      let p = mk_atom st f in
      if Atom.is_true p then ()
      else if Atom.is_false p then (
        let lits, proof = mk_expl() in
        let l = List.rev_map (fun f -> Atom.neg @@ mk_atom st f) lits in
        check_consequence_lits_false_ l;
        let c = Clause.make_removable (p :: l) (Lemma proof) in
        raise_notrace (Th_conflict c)
      ) else (
        insert_var_order st p.var;
        let c = lazy (
          let lits, proof = mk_expl () in
          let l = List.rev_map (fun f -> Atom.neg @@ mk_atom st f) lits in
          (* note: we can check that invariant here in the [lazy] block,
             as conflict analysis will run in an environment where
             the literals should be true anyway, since it's an extension of the
             current trail
             (otherwise the propagated lit would have been backtracked and
             discarded already.) *)
          check_consequence_lits_false_ l;
          Clause.make_removable (p :: l) (Lemma proof)
        ) in
        let level = decision_level st in
        enqueue_bool st p ~level (Bcp_lazy c)
      )

  let[@specialise] acts_iter st ~full head f : unit =
    for i = (if full then 0 else head) to Vec.size st.trail-1 do
      let e = match Vec.get st.trail i with
        | Atom a ->
          Solver_intf.Lit a.lit
        | Lit {term; assigned = Some v; _} ->
          Solver_intf.Assign (term, v)
        | Lit _ -> assert false
      in
      f e
    done

  let eval_atom_ a =
    if Atom.is_true a then Solver_intf.L_true
    else if Atom.is_false a then Solver_intf.L_false
    else Solver_intf.L_undefined

  let[@inline] acts_eval_lit st (f:formula) : Solver_intf.lbool =
    let a = create_atom st f in
    eval_atom_ a

  let[@inline] acts_mk_lit st ?default_pol f : unit =
    ignore (create_atom ?default_pol st f : atom)

  let[@inline] acts_mk_term st t : unit = make_term st t

  let[@inline] current_slice st : _ Solver_intf.acts = {
    Solver_intf.
    acts_iter_assumptions=acts_iter st ~full:false st.th_head;
    acts_eval_lit= acts_eval_lit st;
    acts_mk_lit=acts_mk_lit st;
    acts_mk_term=acts_mk_term st;
    acts_add_clause = acts_add_clause st;
    acts_propagate = acts_propagate st;
    acts_raise_conflict=acts_raise st;
    acts_add_decision_lit=acts_add_decision_lit st;
  }

  (* full slice, for [if_sat] final check *)
  let[@inline] full_slice st : _ Solver_intf.acts = {
    Solver_intf.
    acts_iter_assumptions=acts_iter st ~full:true st.th_head;
    acts_eval_lit= acts_eval_lit st;
    acts_mk_lit=acts_mk_lit st;
    acts_mk_term=acts_mk_term st;
    acts_add_clause = acts_add_clause st;
    acts_propagate = acts_propagate st;
    acts_raise_conflict=acts_raise st;
    acts_add_decision_lit=acts_add_decision_lit st;
  }

  (* Assert that the conflict is indeeed a conflict *)
  let check_is_conflict_ (c:Clause.t) : unit =
    if not @@ Array.for_all (Atom.is_false) c.atoms then (
      invalid_argf "conflict should be false: %a" Clause.debug c
    )

  (* some boolean literals were decided/propagated within Msat. Now we
     need to inform the theory of those assumptions, so it can do its job.
     @return the conflict clause, if the theory detects unsatisfiability *)
  let rec theory_propagate st : clause option =
    assert (st.elt_head = Vec.size st.trail);
    assert (st.th_head <= st.elt_head);
    if st.th_head = st.elt_head then (
      None (* fixpoint/no propagation *)
    ) else (
      let slice = current_slice st in
      st.th_head <- st.elt_head; (* catch up *)
      match Plugin.partial_check st.th slice with
      | () ->
        flush_clauses st;
        propagate st
      | exception Th_conflict c ->
        check_is_conflict_ c;
        Array.iter (fun a -> insert_elt_order st (Elt.of_var a.var)) c.atoms;
        Some c
    )

  (* fixpoint between boolean propagation and theory propagation
     @return a conflict clause, if any *)
  and propagate (st:t) : clause option =
    (* First, treat the stack of lemmas added by the theory, if any *)
    flush_clauses st;
    (* Now, check that the situation is sane *)
    assert (st.elt_head <= Vec.size st.trail);
    if st.elt_head = Vec.size st.trail then (
      theory_propagate st
    ) else (
      match
        while st.elt_head < Vec.size st.trail do
          begin match Vec.get st.trail st.elt_head with
            | Lit _ -> ()
            | Atom a -> propagate_atom st a
          end;
          st.elt_head <- st.elt_head + 1;
        done;
      with
      | () -> theory_propagate st
      | exception Conflict c -> Some c
    )

  (* compute unsat core from assumption [a] *)
  let analyze_final (self:t) (a:atom) : atom list =
    Log.debugf 5 (fun k->k "(@[sat.analyze-final@ :lit %a@])" Atom.debug a);
    assert (Atom.is_false a);
    let core = ref [a] in
    let idx = ref (Vec.size self.trail - 1) in
    Var.mark a.var;
    let seen = ref [a.var] in
    while !idx >= 0 do
      begin match Vec.get self.trail !idx with
        | Lit _ -> () (* semantic decision, ignore *)
        | Atom a' ->
          if Var.marked a'.var then (
            match Atom.reason a' with
            | Some Semantic -> ()
            | Some Decision -> core := a' :: !core
            | Some (Bcp c | Bcp_lazy (lazy c)) ->
              Array.iter
                (fun a ->
                   let v = a.var in
                   if not @@ Var.marked v then (
                     seen := v :: !seen;
                     Var.mark v;
                   ))
                c.atoms
            | None -> ()
          );
      end;
      decr idx
    done;
    List.iter Var.unmark !seen;
    Log.debugf 5 (fun k->k "(@[sat.analyze-final.done@ :core %a@])" (Format.pp_print_list Atom.debug) !core);
    !core

  (* remove some learnt clauses. *)
  let reduce_db (st:t) (n_of_learnts: int) : unit =
    let v = st.clauses_learnt in
    Log.debugf 3 (fun k->k "(@[sat.gc.start :keep %d :out-of %d@])" n_of_learnts (Vec.size v));
    assert (Vec.size v > n_of_learnts);
    (* sort by decreasing activity *)
    Vec.sort v (fun c1 c2 -> compare c2.activity c1.activity);
    let n_collected = ref 0 in
    while Vec.size v > n_of_learnts do
      let c = Vec.pop v in
      assert (Clause.removable c);
      Clause.set_dead c;
      assert (Clause.dead c);
      incr n_collected;
    done;
    Log.debugf 3 (fun k->k "(@[sat.gc.done :collected %d@])" !n_collected);
    ()

  (* Decide on a new literal, and enqueue it into the trail *)
  let rec pick_branch_aux st atom : unit =
    let v = atom.var in
    if v.v_level >= 0 then (
      assert (v.pa.is_true || v.na.is_true);
      pick_branch_lit st
    ) else if Plugin.mcsat then (
      match Plugin.eval st.th atom.lit with
      | Solver_intf.Unknown ->
        new_decision_level st;
        let current_level = decision_level st in
        enqueue_bool st atom ~level:current_level Decision;
        (match st.on_decision with Some f -> f atom | None -> ());
      | Solver_intf.Valued (b, l) ->
        let a = if b then atom else atom.neg in
        enqueue_semantic st a l
    ) else (
      new_decision_level st;
      let current_level = decision_level st in
      enqueue_bool st atom ~level:current_level Decision;
      (match st.on_decision with Some f -> f atom | None -> ());
    )

  and pick_branch_lit st =
    match st.next_decisions with
    | atom :: tl ->
      st.next_decisions <- tl;
      pick_branch_aux st atom
    | [] when decision_level st < Vec.size st.assumptions ->
      (* use an assumption *)
      let a = Vec.get st.assumptions (decision_level st) in
      if Atom.is_true a then (
        new_decision_level st; (* pseudo decision level, [a] is already true *)
        pick_branch_lit st
      ) else if Atom.is_false a then (
        (* root conflict, find unsat core *)
        let core = analyze_final st a in
        raise (E_unsat (US_local {first=a; core}))
      ) else (
        pick_branch_aux st a
      )
    | [] ->
      begin match H.remove_min st.order with
        | E_lit l ->
          if Lit.level l >= 0 then (
            pick_branch_lit st
          ) else (
            let value = Plugin.assign st.th l.term in
            new_decision_level st;
            let current_level = decision_level st in
            enqueue_assign st l value current_level
          )
        | E_var v ->
          pick_branch_aux st (if Var.default_pol v then v.pa else v.na)
        | exception Not_found -> raise_notrace E_sat
      end

  (* do some amount of search, until the number of conflicts or clause learnt
     reaches the given parameters *)
  let search (st:t) n_of_conflicts n_of_learnts : unit =
    Log.debugf 3
      (fun k->k "(@[sat.search@ :n-conflicts %d@ :n-learnt %d@])" n_of_conflicts n_of_learnts);
    let conflictC = ref 0 in
    while true do
      match propagate st with
      | Some confl -> (* Conflict *)
        incr conflictC;
        (* When the theory has raised Unsat, add_boolean_conflict
           might 'forget' the initial conflict clause, and only add the
           analyzed backtrack clause. So in those case, we use add_clause
           to make sure the initial conflict clause is also added. *)
        if Clause.attached confl then (
          add_boolean_conflict st confl
        ) else (
          add_clause_ st confl
        );
        (match st.on_conflict with Some f -> f confl.atoms | None -> ());

      | None -> (* No Conflict *)
        assert (st.elt_head = Vec.size st.trail);
        assert (st.elt_head = st.th_head);
        if n_of_conflicts > 0 && !conflictC >= n_of_conflicts then (
          Log.debug info "(sat.restarting)";
          cancel_until st 0;
          raise_notrace Restart
        );
        (* if decision_level() = 0 then simplify (); *)

        if n_of_learnts > 0 &&
           Vec.size st.clauses_learnt - Vec.size st.trail > n_of_learnts then (
          reduce_db st n_of_learnts;
        );

        pick_branch_lit st
    done

  let eval_level (_st:t) (a:atom) =
    let lvl = a.var.v_level in
    if Atom.is_true a then (
      assert (lvl >= 0);
      true, lvl
    ) else if Atom.is_false a then (
      false, lvl
    ) else (
      raise UndecidedLit
    )

  let[@inline] eval st lit = fst @@ eval_level st lit

  let[@inline] unsat_conflict st = st.unsat_at_0

  let model (st:t) : (term * value) list =
    let opt = function Some a -> a | None -> assert false in
    Vec.fold
      (fun acc e -> match e with
         | Lit v -> (v.term, opt v.assigned)  :: acc
         | Atom _ -> acc)
      [] st.trail

  (* fixpoint of propagation and decisions until a model is found, or a
     conflict is reached *)
  let solve_ (st:t) : unit =
    Log.debugf 5 (fun k->k "(@[sat.solve :assms %d@])" (Vec.size st.assumptions));
    check_unsat_ st;
    try
      flush_clauses st; (* add initial clauses *)
      let n_of_conflicts = ref (float_of_int restart_first) in
      let n_of_learnts = ref ((float_of_int (nb_clauses st)) *. learntsize_factor) in
      while true do
        begin try
            search st (int_of_float !n_of_conflicts) (int_of_float !n_of_learnts)
          with
          | Restart ->
            n_of_conflicts := !n_of_conflicts *. restart_inc;
            n_of_learnts   := !n_of_learnts *. learntsize_inc
          | E_sat ->
            assert (st.elt_head = Vec.size st.trail &&
                    Vec.is_empty st.clauses_to_add &&
                    st.next_decisions=[]);
            begin match Plugin.final_check st.th (full_slice st) with
              | () ->
                if st.elt_head = Vec.size st.trail &&
                   Vec.is_empty st.clauses_to_add &&
                   st.next_decisions = []
                then (
                  raise_notrace E_sat
                );
                (* otherwise, keep on *)
                flush_clauses st;
              | exception Th_conflict c ->
                check_is_conflict_ c;
                Array.iter (fun a -> insert_elt_order st (Elt.of_var a.var)) c.atoms;
                Log.debugf info (fun k -> k "(@[sat.theory-conflict-clause@ %a@])" Clause.debug c);
                (match st.on_conflict with Some f -> f c.atoms | None -> ());
                Vec.push st.clauses_to_add c;
                flush_clauses st;
            end;
        end
      done
    with E_sat -> ()

  let assume st cnf lemma =
    List.iter
      (fun l ->
         let atoms = List.rev_map (mk_atom st) l in
         let c = Clause.make_permanent atoms (Hyp lemma) in
         Log.debugf debug (fun k -> k "(@[sat.assume-clause@ @[<hov 2>%a@]@])" Clause.debug c);
         Vec.push st.clauses_to_add c)
      cnf

  (* Check satisfiability *)
  let check_clause c =
    let res = Array.exists (fun a -> a.is_true) c.atoms in
    if not res then (
      Log.debugf debug
        (fun k -> k "(@[sat.check-clause@ :not-satisfied @[<hov>%a@]@])" Clause.debug c);
      false
    ) else
      true

  let check_vec v = Vec.for_all check_clause v
  let check st : bool =
    Vec.is_empty st.clauses_to_add &&
    check_vec st.clauses_hyps &&
    check_vec st.clauses_learnt

  let[@inline] theory st = st.th

  (* Unsafe access to internal data *)

  let hyps env = env.clauses_hyps

  let history env = env.clauses_learnt

  let trail env = env.trail

  (* Result type *)
  type res =
    | Sat of (term,Formula.t,value) Solver_intf.sat_state
    | Unsat of (atom,clause,Proof.t) Solver_intf.unsat_state

  let pp_all st lvl status =
    Log.debugf lvl
      (fun k -> k
          "(@[<v>sat.full-state :res %s - Full summary:@,@[<hov 2>Trail:@\n%a@]@,\
           @[<hov 2>Hyps:@\n%a@]@,@[<hov 2>Lemmas:@\n%a@]@,@]@."
          status
          (Vec.pp ~sep:"" Trail_elt.debug) (trail st)
          (Vec.pp ~sep:"" Clause.debug) (hyps st)
          (Vec.pp ~sep:"" Clause.debug) (history st)
      )

  let mk_sat (st:t) : (Term.t, Formula.t, _) Solver_intf.sat_state =
    pp_all st 99 "SAT";
    let t = trail st in
    let iter_trail f f' =
      Vec.iter (function
          | Atom a -> f (Atom.formula a)
          | Lit l -> f' l.term)
        t
    in
    let[@inline] eval f = eval st (mk_atom st f) in
    let[@inline] eval_level f = eval_level st (mk_atom st f) in
    { Solver_intf.
      eval; eval_level; iter_trail;
      model = (fun () -> model st);
    }

  let mk_unsat (st:t) (us: unsat_cause) : _ Solver_intf.unsat_state =
    pp_all st 99 "UNSAT";
    let unsat_assumptions () = match us with
      | US_local {first=_; core} -> core
      | _ -> []
    in
    let unsat_conflict = match us with
      | US_false c -> fun() -> c
      | US_local {core=[]; _} -> assert false
      | US_local {first; core} ->
        let c = lazy (
          let core = List.rev core in (* increasing trail order *)
          assert (Atom.equal first @@ List.hd core);
          let proof_of (a:atom) = match Atom.reason a with
            | Some (Decision | Semantic) -> Clause.make_removable [a] Local
            | Some (Bcp c | Bcp_lazy (lazy c)) -> c
            | None -> assert false
          in
          let other_lits = List.filter (fun a -> not (Atom.equal a first)) core in
          let hist =
            Clause.make_permanent [first] Local ::
            proof_of first ::
            List.map proof_of other_lits in
          Clause.make_permanent [] (History hist)
        ) in
        fun () -> Lazy.force c
    in
    let get_proof () =
      let c = unsat_conflict () in
      Proof.prove c
    in
    { Solver_intf.unsat_conflict; get_proof; unsat_assumptions; }

  let add_clause_a st c lemma : unit =
    try
      let c = Clause.make_a ~flags:0 c (Hyp lemma) in
      add_clause_ st c
    with
    | E_unsat (US_false c) ->
      st.unsat_at_0 <- Some c

  let add_clause st c lemma : unit =
    try
      let c = Clause.make_permanent c (Hyp lemma) in
      add_clause_ st c
    with
    | E_unsat (US_false c) ->
      st.unsat_at_0 <- Some c

  let solve ?(assumptions=[]) (st:t) : res =
    cancel_until st 0;
    Vec.clear st.assumptions;
    List.iter (Vec.push st.assumptions) assumptions;
    try
      solve_ st;
      Sat (mk_sat st)
    with E_unsat us ->
      Unsat (mk_unsat st us)

  let true_at_level0 st a =
    try
      let b, lev = eval_level st a in
      b && lev = 0
    with UndecidedLit -> false

  let[@inline] eval_atom _st a : Solver_intf.lbool = eval_atom_ a

  let export (st:t) : clause Solver_intf.export =
    let hyps = hyps st in
    let history = history st in
    {Solver_intf.hyps; history; }
end
[@@inline][@@specialise]

  module Void_ = struct
    type t = Solver_intf.void
    let equal _ _ = assert false
    let hash _ =  assert false
    let pp _ _ = assert false
  end

module Make_cdcl_t(Plugin : Solver_intf.PLUGIN_CDCL_T) =
  Make(struct
    include Plugin
    module Term = Void_
    module Value = Void_
    let eval _ _ = Solver_intf.Unknown
    let assign _ t = t
    let mcsat = false
    let has_theory = true
    let iter_assignable _ _ _ = ()
  end)
[@@inline][@@specialise]

module Make_mcsat(Plugin : Solver_intf.PLUGIN_MCSAT) =
  Make(struct
    include Plugin
    let mcsat = true
    let has_theory = false
  end)
[@@inline][@@specialise]

module Make_pure_sat(Plugin : Solver_intf.PLUGIN_SAT) =
  Make(struct
  module Formula = Plugin.Formula
  module Term = Void_
  module Value = Void_
  type t = unit
  type proof = Plugin.proof
  let push_level () = ()
  let pop_levels _ _ = ()
  let partial_check () _ = ()
  let final_check () _ = ()
  let eval () _ = Solver_intf.Unknown
  let assign () t = t
  let mcsat = false
  let has_theory = false
  let iter_assignable () _ _ = ()
  let mcsat = false
end)
[@@inline][@@specialise]

