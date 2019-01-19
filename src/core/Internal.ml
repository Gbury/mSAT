(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module type PLUGIN = sig
  val mcsat : bool
  (** Is this a mcsat plugin? *)

  include Solver_intf.PLUGIN_MCSAT
end

module Make(Plugin : PLUGIN)
= struct
  module Term = Plugin.Term
  module Formula = Plugin.Formula

  type term = Term.t
  type formula = Formula.t
  type theory = Plugin.t
  type lemma = Plugin.proof

  (* MCSAT literal *)
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
    name : int;
    tag : int option; (* TODO remove *)
    atoms : atom array;
    mutable cpremise : premise;
    mutable activity : float;
    mutable attached : bool; (* TODO: use an int field *)
    mutable visited : bool;
  }

  and reason =
    | Decision
    | Bcp of clause
    | Semantic

  (* TODO: remove, replace with user-provided proof trackng device?
     for pure SAT, [reason] is sufficient *)
  and premise =
    | Hyp
    | Local
    | Lemma of lemma
    | History of clause list

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
    | Hyp -> "H" ^ string_of_int c.name
    | Local -> "L" ^ string_of_int c.name
    | Lemma _ -> "T" ^ string_of_int c.name
    | History _ -> "C" ^ string_of_int c.name

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
        Format.fprintf fmt "@[<hov>@@%d->@ %a@]" v.l_level Term.pp t

    let pp out v = Term.pp out v.term
    let debug out v =
      Format.fprintf out "%d[%a][lit:@[<hov>%a@]]"
        (v.lid+1) debug_assign v Term.pp v.term
  end

  let seen_pos = 0b1
  let seen_neg = 0b10

  module Var = struct
    type t = var
    let[@inline] level v = v.v_level
    let[@inline] pos v = v.pa
    let[@inline] neg v = v.na
    let[@inline] reason v = v.reason
    let[@inline] assignable v = v.v_assignable
    let[@inline] weight v = v.v_weight

    let make (st:st) (t:formula) : var * Solver_intf.negated =
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
    let[@inline] compare a b = Pervasives.compare a.aid b.aid
    let[@inline] reason a = Var.reason a.var
    let[@inline] id a = a.aid
    let[@inline] is_true a = a.is_true
    let[@inline] is_false a = a.neg.is_true

    let[@inline] seen a =
      let pos = equal a (abs a) in
      if pos
      then (seen_pos land a.var.v_fields <> 0)
      else (seen_neg land a.var.v_fields <> 0)

    let[@inline] mark a =
      let pos = equal a (abs a) in
      if pos then (
        a.var.v_fields <- seen_pos lor a.var.v_fields
      ) else (
        a.var.v_fields <- seen_neg lor a.var.v_fields
      )

    let[@inline] make st lit =
      let var, negated = Var.make st lit in
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
    let[@inline] equal c1 c2 = c1==c2
    let[@inline] atoms c = c.atoms
    let[@inline] atoms_l c = Array.to_list c.atoms
    let[@inline] tag c = c.tag
    let hash cl = Array.fold_left (fun i a -> Hashtbl.hash (a.aid, i)) 0 cl.atoms

    let[@inline] premise c = c.cpremise
    let[@inline] set_premise c p = c.cpremise <- p

    let[@inline] visited c = c.visited
    let[@inline] set_visited c b = c.visited <- b

    let[@inline] attached c = c.attached
    let[@inline] set_attached c b = c.attached <- b

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
      | Hyp -> Format.fprintf out "hyp"
      | Local -> Format.fprintf out "local"
      | Lemma _ -> Format.fprintf out "th_lemma"
      | History v ->
        List.iter (fun c -> Format.fprintf out "%s,@ " (name_of_clause c)) v

    let debug out ({atoms=arr; cpremise=cp;_}as c) =
      Format.fprintf out "%s@[<hov>{@[<hov>%a@]}@ cpremise={@[<hov>%a@]}@]"
        (name c) Atom.debug_a arr debug_premise cp
  end

  module Proof =  struct
    exception Insuficient_hyps
    exception Resolution_error of string

    type atom = Atom.t
    type clause = Clause.t
    type formula = Formula.t
    type lemma = Plugin.proof

    let merge = List.merge Atom.compare

    let _c = ref 0
    let fresh_pcl_name () = incr _c; "R" ^ (string_of_int !_c)

    (* Compute resolution of 2 clauses *)
    let resolve l =
      let rec aux resolved acc = function
        | [] -> resolved, acc
        | [a] -> resolved, a :: acc
        | a :: b :: r ->
          if Atom.equal a b then
            aux resolved (a :: acc) r
          else if Atom.equal a.neg b then
            aux (a.var.pa :: resolved) acc r
          else
            aux resolved (a :: acc) (b :: r)
      in
      let resolved, new_clause = aux [] [] l in
      resolved, List.rev new_clause

    (* Compute the set of doublons of a clause *)
    let list c = List.sort Atom.compare (Array.to_list c.atoms)

    let analyze cl =
      let rec aux duplicates free = function
        | [] -> duplicates, free
        | [ x ] -> duplicates, x :: free
        | x :: ((y :: r) as l) ->
          if x == y then
            count duplicates (x :: free) x [y] r
          else
            aux duplicates (x :: free) l
      and count duplicates free x acc = function
        | (y :: r) when x == y ->
          count duplicates free x (y :: acc) r
        | l ->
          aux (acc :: duplicates) free l
      in
      let doublons, acc = aux [] [] cl in
      doublons, List.rev acc

    let to_list c =
      let cl = list c in
      let doublons, l = analyze cl in
      let conflicts, _ = resolve l in
      if doublons <> [] then
        Log.debug 3 "Input clause has redundancies";
      if conflicts <> [] then
        Log.debug 3 "Input clause is a tautology";
      cl

    (* Comparison of clauses *)
    let cmp_cl c d =
      let rec aux = function
        | [], [] -> 0
        | a :: r, a' :: r' ->
          begin match Atom.compare a a' with
            | 0 -> aux (r, r')
            | x -> x
          end
        | _ :: _ , [] -> -1
        | [], _ :: _ -> 1
      in
      aux (c, d)

    let[@inline] prove conclusion =
      assert (conclusion.cpremise <> History []);
      conclusion

    let rec set_atom_proof a =
      let aux acc b =
        if Atom.equal a.neg b then acc
        else set_atom_proof b :: acc
      in
      assert (a.var.v_level >= 0);
      match (a.var.reason) with
      | Some (Bcp c) ->
        Log.debugf 5 (fun k->k "Analysing: @[%a@ %a@]" Atom.debug a Clause.debug c);
        if Array.length c.atoms = 1 then (
          Log.debugf 5 (fun k -> k "Old reason: @[%a@]" Atom.debug a);
          c
        ) else (
          assert (a.neg.is_true);
          let r = History (c :: (Array.fold_left aux [] c.atoms)) in
          let c' = Clause.make [a.neg] r in
          a.var.reason <- Some (Bcp c');
          Log.debugf 5
            (fun k -> k "New reason: @[%a@ %a@]" Atom.debug a Clause.debug c');
          c'
        )
      | _ ->
        Log.debugf 0 (fun k -> k "Error while proving atom %a" Atom.debug a);
        raise (Resolution_error "Cannot prove atom")

    let prove_unsat conflict =
      if Array.length conflict.atoms = 0 then conflict
      else (
        Log.debugf 1 (fun k -> k "Proving unsat from: @[%a@]" Clause.debug conflict);
        let l = Array.fold_left (fun acc a -> set_atom_proof a :: acc) [] conflict.atoms in
        let res = Clause.make [] (History (conflict :: l)) in
        Log.debugf 1 (fun k -> k "Proof found: @[%a@]" Clause.debug res);
        res
      )

    let prove_atom a =
      if (a.is_true && a.var.v_level = 0) then
        Some (set_atom_proof a)
      else
        None

    (* Interface exposed *)
    type t = clause
    and proof_node = {
      conclusion : clause;
      step : step;
    }
    and step =
      | Hypothesis
      | Assumption
      | Lemma of lemma
      | Duplicate of t * atom list
      | Resolution of t * t * atom

    let rec chain_res (c, cl) = function
      | d :: r ->
        Log.debugf 5 
          (fun k -> k "  Resolving clauses : @[%a@\n%a@]" Clause.debug c Clause.debug d);
        let dl = to_list d in
        begin match resolve (merge cl dl) with
          | [ a ], l ->
            begin match r with
              | [] -> (l, c, d, a)
              | _ ->
                let new_clause = Clause.make l (History [c; d]) in
                chain_res (new_clause, l) r
            end
          | _ ->
            Log.debugf 5
              (fun k -> k "While resolving clauses:@[<hov>%a@\n%a@]"
                  Clause.debug c Clause.debug d);
            raise (Resolution_error "Clause mismatch")
        end
      | _ ->
        raise (Resolution_error "Bad history")

    let[@inline] conclusion (p:t) : clause = p

    let expand conclusion =
      Log.debugf 5 (fun k -> k "Expanding : @[%a@]" Clause.debug conclusion);
      match conclusion.cpremise with
      | Lemma l ->
        {conclusion; step = Lemma l; }
      | Hyp ->
        { conclusion; step = Hypothesis; }
      | Local ->
        { conclusion; step = Assumption; }
      | History [] ->
        Log.debugf 0 (fun k -> k "Empty history for clause: %a" Clause.debug conclusion);
        raise (Resolution_error "Empty history")
      | History [ c ] ->
        let duplicates, res = analyze (list c) in
        assert (cmp_cl res (list conclusion) = 0);
        { conclusion; step = Duplicate (c, List.concat duplicates) }
      | History ( c :: ([_] as r)) ->
        let (l, c', d', a) = chain_res (c, to_list c) r in
        assert (cmp_cl l (to_list conclusion) = 0);
        { conclusion; step = Resolution (c', d', a); }
      | History ( c :: r ) ->
        let (l, c', d', a) = chain_res (c, to_list c) r in
        conclusion.cpremise <- History [c'; d'];
        assert (cmp_cl l (to_list conclusion) = 0);
        { conclusion; step = Resolution (c', d', a); }

    (* Proof nodes manipulation *)
    let is_leaf = function
      | Hypothesis
      | Assumption
      | Lemma _ -> true
      | Duplicate _
      | Resolution _ -> false

    let parents = function
      | Hypothesis
      | Assumption
      | Lemma _ -> []
      | Duplicate (p, _) -> [p]
      | Resolution (p, p', _) -> [p; p']

    let expl = function
      | Hypothesis -> "hypothesis"
      | Assumption -> "assumption"
      | Lemma _ -> "lemma"
      | Duplicate _ -> "duplicate"
      | Resolution _ -> "resolution"

    (* Compute unsat-core
       TODO: replace visited bool by a int unique to each call
       of unsat_core, so that the cleanup can be removed ? *)
    let unsat_core proof =
      let rec aux res acc = function
        | [] -> res, acc
        | c :: r ->
          if not c.visited then (
            c.visited <- true;
            match c.cpremise with
            | Hyp | Local | Lemma _ -> aux (c :: res) acc r
            | History h ->
              let l = List.fold_left (fun acc c ->
                  if not c.visited then c :: acc else acc) r h in
              aux res (c :: acc) l
          ) else (
            aux res acc r
          )
      in
      let res, tmp = aux [] [] [proof] in
      List.iter (fun c -> c.visited <- false) res;
      List.iter (fun c -> c.visited <- false) tmp;
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
            | Resolution (p1, p2, _) ->
              Stack.push (Enter p2) s;
              Stack.push (Enter p1) s
            | Hypothesis | Assumption | Lemma _ -> ()
          end
        end;
        fold_aux s h f acc

    let fold f acc p =
      let h = Tbl.create 42 in
      let s = Stack.create () in
      Stack.push (Enter p) s;
      fold_aux s h f acc

    let check p = fold (fun () _ -> ()) () p
  end
  type proof = Proof.t

  module H = (Heap.Make [@specialise]) (struct
    type t = Elt.t
    let[@inline] cmp i j = Elt.weight j < Elt.weight i (* comparison by weight *)
    let idx = Elt.idx
    let set_idx = Elt.set_idx
  end)

  exception E_sat
  exception E_unsat
  exception UndecidedLit
  exception Restart
  exception Conflict of clause

  (* Log levels *)
  let error = 1
  let warn = 3
  let info = 5
  let debug = 50

  let var_decay : float = 1. /. 0.95
  (* inverse of the activity factor for variables. Default 1/0.999 *)

  let clause_decay : float = 1. /. 0.999
  (* inverse of the activity factor for clauses. Default 1/0.95 *)

  let restart_inc : float = 1.5
  (* multiplicative factor for restart limit, default 1.5 *)

  let learntsize_inc : float = 1.1
  (* multiplicative factor for [learntsize_factor] at each restart, default 1.1 *)

  (* Singleton type containing the current state *)
  type t = {
    st : st;
    th: theory;

    (* Clauses are simplified for eficiency purposes. In the following
       vectors, the comments actually refer to the original non-simplified
       clause. *)

    clauses_hyps : clause Vec.t;
    (* clauses added by the user *)
    clauses_learnt : clause Vec.t;
    (* learnt clauses (tautologies true at any time, whatever the user level) *)
    (* TODO: remove, replace by vec of assumptions *)
    clauses_temp : clause Vec.t;
    (* Temp clauses, corresponding to the local assumptions. This vec is used
       only to have an efficient way to access the list of local assumptions. *)

    clauses_root : clause Stack.t;
    (* Clauses that should propagate at level 0, but couldn't *)
    clauses_to_add : clause Stack.t;
    (* Clauses either assumed or pushed by the theory, waiting to be added. *)


    mutable unsat_conflict : clause option;
    (* conflict clause at [base_level], if any *)
    mutable next_decision : atom option;
    (* When the last conflict was a semantic one, this stores the next decision to make *)

    trail : trail_elt Vec.t;
    (* decision stack + propagated elements (atoms or assignments). *)

    elt_levels : int Vec.t;
    (* decision levels in [trail]  *)
    th_levels : Plugin.level Vec.t;
    (* theory states corresponding to elt_levels *)

    (* TODO: remove *)
    user_levels : int Vec.t;
    (* user levels in [clauses_temp] *)

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

    mutable var_incr : float;
    (* increment for variables' activity *)

    mutable clause_incr : float;
    (* increment for clauses' activity *)

    mutable restart_first : int;
    (* intial restart limit, default 100 *)

    mutable learntsize_factor : float;
    (* initial limit for the number of learnt clauses, 1/3 of initial
        number of clauses by default *)

    (* TODO: remove *)
    mutable dirty: bool;
    (* is there a [pop()] on top of the stack for examining
       current model/proof? *)
  }
  type solver = t

  (* Starting environment. *)
  let create_ ~st (th:theory) : t = {
    st; th;
    unsat_conflict = None;
    next_decision = None;

    clauses_hyps = Vec.create();
    clauses_learnt = Vec.create();
    clauses_temp = Vec.create();

    clauses_root = Stack.create ();
    clauses_to_add = Stack.create ();

    th_head = 0;
    elt_head = 0;

    trail = Vec.create ();
    elt_levels = Vec.create();
    th_levels = Vec.create();
    user_levels = Vec.create();

    order = H.create();

    var_incr = 1.;
    clause_incr = 1.;

    restart_first = 100;

    learntsize_factor = 1. /. 3. ;
    dirty=false;
  }

  let create ?(size=`Big) (th:theory) : t =
    let st = create_st ~size () in
    create_ ~st th

  (* Misc functions *)
  let to_float = float_of_int
  let to_int = int_of_float

  let[@inline] st t = t.st
  let[@inline] nb_clauses st = Vec.size st.clauses_hyps
  (* let nb_vars    () = St.nb_elt () *)

  let[@inline] decision_level st = Vec.size st.elt_levels
  let[@inline] base_level st = Vec.size st.user_levels

  (* Are the assumptions currently unsat ? *)
  let[@inline] is_unsat st =
    match st.unsat_conflict with
    | Some _ -> true
    | None -> false

  (* Iteration over subterms.
     When incrementing activity, we want to be able to iterate over
     all subterms of a formula. However, the function provided by the theory
     may be costly (if it walks a tree-like structure, and does some processing
     to ignore some subterms for instance), so we want to 'cache' the list
     of subterms of each formula, so we have a field [v_assignable]
     directly in variables to do so.  *)
  let iter_sub f v =
    if Plugin.mcsat then
      match v.v_assignable with
      | Some l -> List.iter f l
      | None -> assert false

  (* When we have a new literal,
     we need to first create the list of its subterms. *)
  let mk_atom st (f:formula) : atom =
    let res = Atom.make st.st f in
    if Plugin.mcsat then (
      begin match res.var.v_assignable with
        | Some _ -> ()
        | None ->
          let l = ref [] in
          Plugin.iter_assignable st.th
            (fun t -> l := Lit.make st.st t :: !l)
            res.var.pa.lit;
          res.var.v_assignable <- Some !l;
      end;
    );
    res

  (* Variable and literal activity.
     Activity is used to decide on which variable to decide when propagation
     is done. Uses a heap (implemented in Iheap), to keep track of variable activity.
     To be more general, the heap only stores the variable/literal id (i.e an int).
     When we add a variable (which wraps a formula), we also need to add all
     its subterms.
  *)
  let rec insert_var_order st (elt:elt) : unit =
    H.insert st.order elt;
    if Plugin.mcsat then (
      match elt with
      | E_lit _ -> ()
      | E_var v -> insert_subterms_order st v
    )

  and insert_subterms_order st (v:var) : unit =
    iter_sub (fun t -> insert_var_order st (Elt.of_lit t)) v

  (* Add new litterals/atoms on which to decide on, even if there is no
     clause that constrains it.
     We could maybe check if they have already has been decided before
     inserting them into the heap, if it appears that it helps performance. *)
  let new_lit st t =
    let l = Lit.make st.st t in
    insert_var_order st (E_lit l)

  let make_atom_ st (p:formula) : atom =
    let a = mk_atom st p in
    insert_var_order st (E_var a.var);
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
  let lit_bump_activity_aux st (l:lit): unit =
    l.l_weight <- l.l_weight +. st.var_incr;
    if l.l_weight > 1e100 then (
      for i = 0 to nb_elt st.st - 1 do
        Elt.set_weight (get_elt st.st i) ((Elt.weight (get_elt st.st i)) *. 1e-100)
      done;
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
      for i = 0 to Vec.size st.clauses_learnt - 1 do
        (Vec.get st.clauses_learnt i).activity <-
          (Vec.get st.clauses_learnt i).activity *. 1e-20;
      done;
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

  (* Eliminates atom doublons in clauses *)
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
    if !trivial then
      raise Trivial
    else if !duplicates = [] then
      clause
    else
      Clause.make !res (History [clause])

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
            | Some (Bcp cl) ->
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
    Vec.push st.th_levels (Plugin.current_level st.th); (* save the current theory state *)
    ()

  (* Attach/Detach a clause.

     A clause is attached (to its watching lits) when it is first added,
     either because it is assumed or learnt.

  *)
  let attach_clause c =
    assert (not c.attached);
    Log.debugf debug (fun k -> k "Attaching %a" Clause.debug c);
    Vec.push c.atoms.(0).neg.watched c;
    Vec.push c.atoms.(1).neg.watched c;
    c.attached <- true;
    ()

  (* Backtracking.
     Used to backtrack, i.e cancel down to [lvl] excluded,
     i.e we want to go back to the state the solver was in
         when decision level [lvl] was created. *)
  let cancel_until st lvl =
    assert (lvl >= base_level st);
    (* Nothing to do if we try to backtrack to a non-existent level. *)
    if decision_level st <= lvl then (
      Log.debugf debug (fun k -> k "Already at level <= %d" lvl)
    ) else (
      Log.debugf info (fun k -> k "Backtracking to lvl %d" lvl);
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
            insert_var_order st (Elt.of_lit l)
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
            insert_var_order st (Elt.of_var a.var)
          )
      done;
      (* Recover the right theory state. *)
      Plugin.backtrack st.th (Vec.get st.th_levels lvl);
      (* Resize the vectors according to their new size. *)
      Vec.shrink st.trail !head;
      Vec.shrink st.elt_levels lvl;
      Vec.shrink st.th_levels lvl;
    );
    assert (Vec.size st.elt_levels = Vec.size st.th_levels);
    ()

  (* Unsatisfiability is signaled through an exception, since it can happen
     in multiple places (adding new clauses, or solving for instance). *)
  let report_unsat st confl : _ =
    Log.debugf info (fun k -> k "@[Unsat conflict: %a@]" Clause.debug confl);
    st.unsat_conflict <- Some confl;
    raise E_unsat

  (* Simplification of boolean propagation reasons.
     When doing boolean propagation *at level 0*, it can happen
     that the clause cl, which propagates a formula, also contains
     other formulas, but has been simplified. in which case, we
     need to rebuild a clause with correct history, in order to
     be able to build a correct proof at the end of proof search. *)
  let simpl_reason : reason -> reason = function
    | (Bcp cl) as r ->
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
            let c' = Clause.make l (History (cl :: history)) in
            Log.debugf debug
              (fun k -> k "Simplified reason: @[<v>%a@,%a@]" Clause.debug cl Clause.debug c');
            Bcp c'
          )
        | _ ->
          Log.debugf error
            (fun k ->
               k "@[<v 2>Failed at reason simplification:@,%a@,%a@]"
                 (Vec.pp ~sep:"" Atom.debug)
                 (Vec.of_list l)
                 Clause.debug cl);
          assert false
      end
    | r -> r

  (* Boolean propagation.
     Wrapper function for adding a new propagated formula. *)
  let enqueue_bool st a ~level:lvl reason : unit =
    if a.neg.is_true then (
      Log.debugf error (fun k->k "Trying to enqueue a false literal: %a" Atom.debug a);
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
      (fun k->k "Enqueue (%d): %a" (Vec.size st.trail) Atom.debug a);
    ()

  let enqueue_semantic st a terms =
    if not a.is_true then (
      let l = List.map (Lit.make st.st) terms in
      let lvl = List.fold_left (fun acc {l_level; _} ->
          assert (l_level > 0); max acc l_level) 0 l in
      enqueue_bool st a ~level:lvl Semantic
    )

  (* MCsat semantic assignment *)
  let enqueue_assign st l value lvl =
    match l.assigned with
    | Some _ ->
      Log.debugf error
        (fun k -> k "Trying to assign an already assigned literal: %a" Lit.debug l);
      assert false
    | None ->
      assert (l.l_level < 0);
      l.assigned <- Some value;
      l.l_level <- lvl;
      Vec.push st.trail (Trail_elt.of_lit l);
      Log.debugf debug
        (fun k -> k "Enqueue (%d): %a" (Vec.size st.trail) Lit.debug l);
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
        if l = [] then
          raise (Invalid_argument (
              Format.asprintf "msat:core/internal.ml: %s"
                "semantic propagation at level 0 are currently forbidden"));
        let atom = if b then a else a.neg in
        enqueue_semantic st atom l;
        Some b

  (* find which level to backtrack to, given a conflict clause
     and a boolean stating whether it is
     a UIP ("Unique Implication Point")
     precond: the atom list is sorted by decreasing decision level *)
  let backtrack_lvl st : atom list -> int * bool = function
    | [] | [_] ->
      0, true
    | a :: b :: _ ->
      assert(a.var.v_level > base_level st);
      if a.var.v_level > b.var.v_level then (
        (* backtrack below [a], so we can propagate [not a] *)
        b.var.v_level, true
      ) else (
        assert (a.var.v_level = b.var.v_level);
        assert (a.var.v_level >= base_level st);
        max (a.var.v_level - 1) (base_level st), false
      )

  (* result of conflict analysis, containing the learnt clause and some
     additional info.

     invariant: cr_history's order matters, as its head is later used
     during pop operations to determine the origin of a clause/conflict
     (boolean conflict i.e hypothesis, or theory lemma) *)
  type conflict_res = {
    cr_backtrack_lvl : int; (* level to backtrack to *)
    cr_learnt: atom list; (* lemma learnt from conflict *)
    cr_history: clause list; (* justification *)
    cr_is_uip: bool; (* conflict is UIP? *)
  }

  let get_atom st i =
    match Vec.get st.trail i with
    | Lit _ -> assert false | Atom x -> x

  (* conflict analysis for SAT
     Same idea as the mcsat analyze function (without semantic propagations),
     except we look the the Last UIP (TODO: check ?), and do it in an imperative
     and efficient manner. *)
  let analyze_sat st c_clause : conflict_res =
    let pathC  = ref 0 in
    let learnt = ref [] in
    let cond   = ref true in
    let blevel = ref 0 in
    let seen   = ref [] in
    let c      = ref (Some c_clause) in
    let tr_ind = ref (Vec.size st.trail - 1) in
    let history = ref [] in
    assert (decision_level st > 0);
    let conflict_level =
      Array.fold_left (fun acc p -> max acc p.var.v_level) 0 c_clause.atoms
    in
    Log.debugf debug
      (fun k -> k "Analyzing conflict (%d): %a" conflict_level Clause.debug c_clause);
    while !cond do
      begin match !c with
        | None ->
          Log.debug debug "  skipping resolution for semantic propagation"
        | Some clause ->
          Log.debugf debug (fun k->k"  Resolving clause: %a"  Clause.debug clause);
          begin match clause.cpremise with
            | History _ -> clause_bump_activity st clause
            | Hyp | Local | Lemma _ -> ()
          end;
          history := clause :: !history;
          (* visit the current predecessors *)
          for j = 0 to Array.length clause.atoms - 1 do
            let q = clause.atoms.(j) in
            assert (q.is_true || q.neg.is_true && q.var.v_level >= 0); (* unsure? *)
            if q.var.v_level <= 0 then (
              assert (q.neg.is_true);
              match q.var.reason with
              | Some Bcp cl -> history := cl :: !history
              | _ -> assert false
            );
            if not (Var.seen_both q.var) then (
              Atom.mark q;
              Atom.mark q.neg;
              seen := q :: !seen;
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
        Log.debugf debug (fun k -> k "  looking at: %a" Trail_elt.debug a);
        match a with
        | Atom q ->
          (not (Var.seen_both q.var)) ||
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
        learnt := p.neg :: (List.rev !learnt)
      | n, Some Semantic ->
        assert (n > 0);
        learnt := p.neg :: !learnt;
        c := None
      | n, Some Bcp cl ->
        assert (n > 0);
        assert (p.var.v_level >= conflict_level);
        c := Some cl
      | _ -> assert false
    done;
    List.iter (fun q -> Var.clear q.var) !seen;
    let l = List.fast_sort (fun p q -> compare q.var.v_level p.var.v_level) !learnt in
    let level, is_uip = backtrack_lvl st l in
    { cr_backtrack_lvl = level;
      cr_learnt = l;
      cr_history = List.rev !history;
      cr_is_uip = is_uip;
    }

  let[@inline] analyze st c_clause : conflict_res =
    analyze_sat st c_clause
      (*
    if mcsat
    then analyze_mcsat c_clause
    else analyze_sat c_clause
         *)

  (* add the learnt clause to the clause database, propagate, etc. *)
  let record_learnt_clause st (confl:clause) (cr:conflict_res): unit =
    begin match cr.cr_learnt with
      | [] -> assert false
      | [fuip] ->
        assert (cr.cr_backtrack_lvl = 0);
        if fuip.neg.is_true then (
          report_unsat st confl
        ) else (
          let uclause = Clause.make cr.cr_learnt (History cr.cr_history) in
          Vec.push st.clauses_learnt uclause;
          (* no need to attach [uclause], it is true at level 0 *)
          enqueue_bool st fuip ~level:0 (Bcp uclause)
        )
      | fuip :: _ ->
        let lclause = Clause.make cr.cr_learnt (History cr.cr_history) in
        Vec.push st.clauses_learnt lclause;
        attach_clause lclause;
        clause_bump_activity st lclause;
        if cr.cr_is_uip then (
          enqueue_bool st fuip ~level:cr.cr_backtrack_lvl (Bcp lclause)
        ) else (
          st.next_decision <- Some fuip.neg
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
    Log.debugf info (fun k -> k "Boolean conflict: %a" Clause.debug confl);
    st.next_decision <- None;
    assert (decision_level st >= base_level st);
    if decision_level st = base_level st ||
       Array.for_all (fun a -> a.var.v_level <= base_level st) confl.atoms then (
      (* Top-level conflict *)
      report_unsat st confl;
    );
    let cr = analyze st confl in
    cancel_until st (max cr.cr_backtrack_lvl (base_level st));
    record_learnt_clause st confl cr

  (* Get the correct vector to insert a clause in. *)
  let clause_vector st c =
    match c.cpremise with
    | Hyp -> st.clauses_hyps
    | Local -> st.clauses_temp
    | Lemma _ | History _ -> st.clauses_learnt

  (* Add a new clause, simplifying, propagating, and backtracking if
     the clause is false in the current trail *)
  let add_clause st (init:clause) : unit =
    Log.debugf debug (fun k -> k "Adding clause: @[<hov>%a@]" Clause.debug init);
    (* Insertion of new lits is done before simplification. Indeed, else a lit in a
       trivial clause could end up being not decided on, which is a bug. *)
    Array.iter (fun x -> insert_var_order st (Elt.of_var x.var)) init.atoms;
    let vec = clause_vector st init in
    try
      let c = eliminate_duplicates init in
      Log.debugf debug (fun k -> k "Doublons eliminated: %a" Clause.debug c);
      let atoms, history = partition c.atoms in
      let clause =
        if history = []
        then (
          (* update order of atoms *)
          List.iteri (fun i a -> c.atoms.(i) <- a) atoms;
          c
        )
        else Clause.make atoms (History (c :: history))
      in
      Log.debugf info (fun k->k "New clause: @[<hov>%a@]" Clause.debug clause);
      match atoms with
      | [] ->
        (* Report_unsat will raise, and the current clause will be lost if we do not
           store it somewhere. Since the proof search will end, any of env.clauses_to_add
           or env.clauses_root is adequate. *)
        Stack.push clause st.clauses_root;
        report_unsat st clause
      | [a]   ->
        cancel_until st (base_level st);
        if a.neg.is_true then (
          (* Since we cannot propagate the atom [a], in order to not lose
             the information that [a] must be true, we add clause to the list
             of clauses to add, so that it will be e-examined later. *)
          Log.debug debug "Unit clause, adding to clauses to add";
          Stack.push clause st.clauses_to_add;
          report_unsat st clause
        ) else if a.is_true then (
          (* If the atom is already true, then it should be because of a local hyp.
             However it means we can't propagate it at level 0. In order to not lose
             that information, we store the clause in a stack of clauses that we will
             add to the solver at the next pop. *)
          Log.debug debug "Unit clause, adding to root clauses";
          assert (0 < a.var.v_level && a.var.v_level <= base_level st);
          Stack.push clause st.clauses_root;
          ()
        ) else (
          Log.debugf debug (fun k->k "Unit clause, propagating: %a" Atom.debug a);
          Vec.push vec clause;
          enqueue_bool st a ~level:0 (Bcp clause)
        )
      | a::b::_ ->
        Vec.push vec clause;
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
            cancel_until st (max lvl (base_level st));
            enqueue_bool st a ~level:lvl (Bcp clause)
          )
        )
    with Trivial ->
      Vec.push vec init;
      Log.debugf info (fun k->k "Trivial clause ignored : @[%a@]" Clause.debug init)

  let flush_clauses st =
    if not (Stack.is_empty st.clauses_to_add) then (
      while not (Stack.is_empty st.clauses_to_add) do
        let c = Stack.pop st.clauses_to_add in
        add_clause st c
      done
    )

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
    ) else assert (a.neg == atoms.(1));
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
            raise Exit
          )
        done;
        (* no watch lit found *)
        if first.neg.is_true then (
          (* clause is false *)
          st.elt_head <- Vec.size st.trail;
          raise (Conflict c)
        ) else (
          match th_eval st first with
          | None -> (* clause is unit, keep the same watches, but propagate *)
            enqueue_bool st first ~level:(decision_level st) (Bcp c)
          | Some true -> ()
          | Some false ->
            st.elt_head <- Vec.size st.trail;
            raise (Conflict c)
        );
        Watch_kept
      with Exit ->
        Watch_removed
    )

  (* propagate atom [a], which was just decided. This checks every
     clause watching [a] to see if the clause is false, unit, or has
     other possible watches
     @param res the optional conflict clause that the propagation might trigger *)
  let propagate_atom st a (res:clause option ref) : unit =
    let watched = a.watched in
    begin
      try
        let rec aux i =
          if i >= Vec.size watched then ()
          else (
            let c = Vec.get watched i in
            assert c.attached;
            let j = match propagate_in_clause st a c i with
              | Watch_kept -> i+1
              | Watch_removed -> i (* clause at this index changed *)
            in
            aux j
          )
        in
        aux 0
      with Conflict c ->
        assert (!res = None);
        res := Some c
    end;
    ()

  (* Propagation (boolean and theory) *)
  let create_atom st f =
    let a = mk_atom st f in
    ignore (th_eval st a);
    a

  let slice_get st i =
    match Vec.get st.trail i with
    | Atom a ->
      Solver_intf.Lit a.lit
    | Lit {term; assigned = Some v; _} ->
      Solver_intf.Assign (term, v)
    | Lit _ -> assert false

  let slice_push st (l:formula list) (lemma:lemma): unit =
    let atoms = List.rev_map (create_atom st) l in
    let c = Clause.make atoms (Lemma lemma) in
    Log.debugf info (fun k->k "Pushing clause %a" Clause.debug c);
    Stack.push c st.clauses_to_add

  let slice_propagate (st:t) f = function
    | Solver_intf.Eval l ->
      let a = mk_atom st f in
      enqueue_semantic st a l
    | Solver_intf.Consequence (causes, proof) ->
      let l = List.rev_map (mk_atom st) causes in
      if List.for_all (fun a -> a.is_true) l then (
        let p = mk_atom st f in
        let c = Clause.make (p :: List.map Atom.neg l) (Lemma proof) in
        if p.is_true then ()
        else if p.neg.is_true then (
          Stack.push c st.clauses_to_add
        ) else (
          insert_subterms_order st p.var;
          let level = List.fold_left (fun acc a -> max acc a.var.v_level) 0 l in
          enqueue_bool st p ~level (Bcp c)
        )
      ) else (
        invalid_arg "Msat.Internal.slice_propagate"
      )

  let current_slice st : (_,_,_) Solver_intf.slice = {
    Solver_intf.start = st.th_head;
    length = Vec.size st.trail - st.th_head;
    get = slice_get st;
    push = slice_push st;
    propagate = slice_propagate st;
  }

  (* full slice, for [if_sat] final check *)
  let full_slice st : (_,_,_) Solver_intf.slice = {
    Solver_intf.start = 0;
    length = Vec.size st.trail;
    get = slice_get st;
    push = slice_push st;
    propagate = (fun _ -> assert false);
  }

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
      match Plugin.assume st.th slice with
      | Solver_intf.Th_sat ->
        propagate st
      | Solver_intf.Th_unsat (l, p) ->
        (* conflict *)
        let l = List.rev_map (create_atom st) l in
        (* Assert that the conflcit is indeeed a conflict *)
        if not @@ List.for_all (fun a -> a.neg.is_true) l then (
          raise (Invalid_argument "msat:core/internal: invalid conflict");
        );
        List.iter (fun a -> insert_var_order st (Elt.of_var a.var)) l;
        (* Create the clause and return it. *)
        let c = Clause.make l (Lemma p) in
        Some c
    )

  (* fixpoint between boolean propagation and theory propagation
     @return a conflict clause, if any *)
  and propagate (st:t) : clause option =
    (* First, treat the stack of lemmas added by the theory, if any *)
    flush_clauses st;
    (* Now, check that the situation is sane *)
    assert (st.elt_head <= Vec.size st.trail);
    if st.elt_head = Vec.size st.trail then
      theory_propagate st
    else (
      let num_props = ref 0 in
      let res = ref None in
      while st.elt_head < Vec.size st.trail do
        begin match Vec.get st.trail st.elt_head with
          | Lit _ -> ()
          | Atom a ->
            incr num_props;
            propagate_atom st a res
        end;
        st.elt_head <- st.elt_head + 1;
      done;
      match !res with
      | None -> theory_propagate st
      | _ -> !res
    )

  (* remove some learnt clauses
     NOTE: so far we do not forget learnt clauses. We could, as long as
     lemmas from the theory itself are kept. *)
  let reduce_db () = ()

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
        enqueue_bool st atom ~level:current_level Decision
      | Solver_intf.Valued (b, l) ->
        let a = if b then atom else atom.neg in
        enqueue_semantic st a l
    ) else (
      new_decision_level st;
      let current_level = decision_level st in
      enqueue_bool st atom ~level:current_level Decision
    )

  and pick_branch_lit st =
    match st.next_decision with
    | Some atom ->
      st.next_decision <- None;
      pick_branch_aux st atom
    | None ->
      begin match H.remove_min st.order with
        | E_lit l ->
          if Lit.level l >= 0 then
            pick_branch_lit st
          else (
            let value = Plugin.assign st.th l.term in
            new_decision_level st;
            let current_level = decision_level st in
            enqueue_assign st l value current_level
          )
        | E_var v ->
          pick_branch_aux st v.pa
        | exception Not_found -> raise E_sat
      end

  (* do some amount of search, until the number of conflicts or clause learnt
     reaches the given parameters *)
  let search (st:t) n_of_conflicts n_of_learnts : unit =
    let conflictC = ref 0 in
    while true do
      match propagate st with
      | Some confl -> (* Conflict *)
        incr conflictC;
        (* When the theory has raised Unsat, add_boolean_conflict
           might 'forget' the initial conflict clause, and only add the
           analyzed backtrack clause. So in those case, we use add_clause
           to make sure the initial conflict clause is also added. *)
        if confl.attached then
          add_boolean_conflict st confl
        else
          add_clause st confl

      | None -> (* No Conflict *)
        assert (st.elt_head = Vec.size st.trail);
        assert (st.elt_head = st.th_head);
        if Vec.size st.trail = nb_elt st.st then raise E_sat;
        if n_of_conflicts > 0 && !conflictC >= n_of_conflicts then (
          Log.debug info "Restarting...";
          cancel_until st (base_level st);
          raise Restart
        );
        (* if decision_level() = 0 then simplify (); *)

        if n_of_learnts >= 0 &&
           Vec.size st.clauses_learnt - Vec.size st.trail >= n_of_learnts
        then reduce_db();

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

  let[@inline] unsat_conflict st = st.unsat_conflict

  let model (st:t) : (term * term) list =
    let opt = function Some a -> a | None -> assert false in
    Vec.fold
      (fun acc e -> match e with
         | Lit v -> (v.term, opt v.assigned)  :: acc
         | Atom _ -> acc)
      [] st.trail

  (* fixpoint of propagation and decisions until a model is found, or a
     conflict is reached *)
  let solve (st:t) : unit =
    Log.debug 5 "solve";
    if is_unsat st then raise E_unsat;
    let n_of_conflicts = ref (to_float st.restart_first) in
    let n_of_learnts = ref ((to_float (nb_clauses st)) *. st.learntsize_factor) in
    try
      while true do
        begin try
            search st (to_int !n_of_conflicts) (to_int !n_of_learnts)
          with
          | Restart ->
            n_of_conflicts := !n_of_conflicts *. restart_inc;
            n_of_learnts   := !n_of_learnts *. learntsize_inc
          | E_sat ->
            assert (st.elt_head = Vec.size st.trail);
            begin match Plugin.if_sat st.th (full_slice st) with
              | Solver_intf.Th_sat -> ()
              | Solver_intf.Th_unsat (l, p) ->
                let atoms = List.rev_map (create_atom st) l in
                let c = Clause.make atoms (Lemma p) in
                Log.debugf info (fun k -> k "Theory conflict clause: %a" Clause.debug c);
                Stack.push c st.clauses_to_add
            end;
            if Stack.is_empty st.clauses_to_add then raise E_sat
        end
      done
    with E_sat -> ()

  let assume st ?tag cnf =
    List.iter
      (fun l ->
         let atoms = List.rev_map (mk_atom st) l in
         let c = Clause.make ?tag atoms Hyp in
         Log.debugf debug (fun k -> k "Assuming clause: @[<hov 2>%a@]" Clause.debug c);
         Stack.push c st.clauses_to_add)
      cnf

  (* create a factice decision level for local assumptions *)
  let push st : unit =
    Log.debug debug "Pushing a new user level";
    match st.unsat_conflict with
    | Some _ -> raise E_unsat
    | None ->
      cancel_until st (base_level st);
      Log.debugf debug
        (fun k -> k "@[<v>Status:@,@[<hov 2>trail: %d - %d@,%a@]"
            st.elt_head st.th_head (Vec.pp ~sep:"" Trail_elt.debug) st.trail);
      begin match propagate st with
        | Some confl ->
          report_unsat st confl
        | None ->
          Log.debugf debug
            (fun k -> k "@[<v>Current trail:@,@[<hov>%a@]@]"
                (Vec.pp ~sep:"" Trail_elt.debug) st.trail);
          Log.debug info "Creating new user level";
          new_decision_level st;
          Vec.push st.user_levels (Vec.size st.clauses_temp);
          assert (decision_level st = base_level st)
      end

  (* pop the last factice decision level *)
  let pop st : unit =
    if base_level st = 0 then
      Log.debug warn "Cannot pop (already at level 0)"
    else (
      Log.debug info "Popping user level";
      assert (base_level st > 0);
      st.unsat_conflict <- None;
      let n = Vec.pop st.user_levels in (* before the [cancel_until]! *)
      (* Add the root clauses to the clauses to add *)
      Stack.iter (fun c -> Stack.push c st.clauses_to_add) st.clauses_root;
      Stack.clear st.clauses_root;
      (* remove from env.clauses_temp the now invalid caluses. *)
      Vec.shrink st.clauses_temp n;
      assert (Vec.for_all (fun c -> Array.length c.atoms = 1) st.clauses_temp);
      assert (Vec.for_all (fun c -> c.atoms.(0).var.v_level <= base_level st) st.clauses_temp);
      cancel_until st (base_level st)
    )

  (* Add local hyps to the current decision level *)
  let local (st:t) (l:atom list) : unit =
    let aux a =
      Log.debugf info (fun k-> k "Local assumption: @[%a@]" Atom.debug a);
      assert (decision_level st = base_level st);
      if not a.is_true then (
        let c = Clause.make [a] Local in
        Log.debugf debug (fun k -> k "Temp clause: @[%a@]" Clause.debug c);
        Vec.push st.clauses_temp c;
        if a.neg.is_true then (
          (* conflict between assumptions: UNSAT *)
          report_unsat st c;
        ) else (
          (* make a decision, propagate *)
          let level = decision_level st in
          enqueue_bool st a ~level (Bcp c);
        )
      )
    in
    assert (base_level st > 0);
    match st.unsat_conflict with
    | None ->
      Log.debug info "Adding local assumption";
      cancel_until st (base_level st);
      List.iter aux l
    | Some _ ->
      Log.debug warn "Cannot add local assumption (already unsat)"

  (* Check satisfiability *)
  let check_clause c =
    let tmp = Array.map (fun a ->
        if a.is_true then true
        else if a.neg.is_true then false
        else raise UndecidedLit) c.atoms in
    let res = Array.exists (fun x -> x) tmp in
    if not res then (
      Log.debugf debug
        (fun k -> k "Clause not satisfied: @[<hov>%a@]" Clause.debug c);
      false
    ) else
      true

  let check_vec v =
    Vec.for_all check_clause v

  let check_stack s =
    try
      Stack.iter (fun c -> if not (check_clause c) then raise Exit) s;
      true
    with Exit ->
      false

  let check st : bool =
    Stack.is_empty st.clauses_to_add &&
    check_stack st.clauses_root &&
    check_vec st.clauses_hyps &&
    check_vec st.clauses_learnt &&
    check_vec st.clauses_temp

  (* Unsafe access to internal data *)

  let hyps env = env.clauses_hyps

  let history env = env.clauses_learnt

  let temp env = env.clauses_temp

  let trail env = env.trail

  (* Result type *)
  type res =
    | Sat of (term,atom) Solver_intf.sat_state
    | Unsat of (clause,Proof.t) Solver_intf.unsat_state

  let pp_all st lvl status =
    Log.debugf lvl
      (fun k -> k
          "@[<v>%s - Full resume:@,@[<hov 2>Trail:@\n%a@]@,\
           @[<hov 2>Temp:@\n%a@]@,@[<hov 2>Hyps:@\n%a@]@,@[<hov 2>Lemmas:@\n%a@]@,@]@."
          status
          (Vec.pp ~sep:"" Trail_elt.debug) (trail st)
          (Vec.pp ~sep:"" Clause.debug) (temp st)
          (Vec.pp ~sep:"" Clause.debug) (hyps st)
          (Vec.pp ~sep:"" Clause.debug) (history st)
      )

  let mk_sat (st:t) : (_,_) Solver_intf.sat_state =
    pp_all st 99 "SAT";
    let t = trail st in
    let iter f f' =
      Vec.iter (function
          | Atom a -> f a
          | Lit l -> f' l.term)
        t
    in
    { Solver_intf.
      eval = eval st;
      eval_level = eval_level st;
      iter_trail = iter;
      model = (fun () -> model st);
    }

  let mk_unsat (st:t) : (_,_) Solver_intf.unsat_state =
    pp_all st 99 "UNSAT";
    let unsat_conflict () =
      match unsat_conflict st with
      | None -> assert false
      | Some c -> c
    in
    let get_proof () =
      let c = unsat_conflict () in
      Proof.prove_unsat c
    in
    { Solver_intf.unsat_conflict; get_proof; }

  (* clean local state *)
  let[@inline] cleanup_ (st:t) : unit =
    if st.dirty then (
      pop st; (* reset *)
      st.dirty <- false;
    )

  (* Wrappers around internal functions*)
  let[@inline] assume st ?tag cls : unit =
    cleanup_ st;
    assume st ?tag cls

  let[@inline] add_clause st ?tag c : unit =
    cleanup_ st;
    let c = Clause.make ?tag c Hyp in
    add_clause st c

  let solve (st:t) ?(assumptions=[]) () =
    cleanup_ st;
    try
      push st;
      st.dirty <- true; (* to call [pop] before any other action *)
      local st assumptions;
      solve st;
      Sat (mk_sat st)
    with E_unsat ->
      Unsat (mk_unsat st)

  let[@inline] push st =
    cleanup_ st;
    push st

  let[@inline] pop st =
    cleanup_ st;
    pop st

  let unsat_core = Proof.unsat_core

  let true_at_level0 st a =
    try
      let b, lev = eval_level st a in
      b && lev = 0
    with UndecidedLit -> false

  (* TODO: remove *)
  let get_tag cl = cl.tag

  let[@inline] new_lit st t =
    cleanup_ st;
    new_lit st t

  let[@inline] make_atom st a =
    cleanup_ st;
    make_atom_ st a

  let export (st:t) : clause Solver_intf.export =
    let hyps = hyps st in
    let history = history st in
    let local = temp st in
    {Solver_intf.hyps; history; local}
end
[@@inline][@@specialise]

module Make_cdcl_t(Plugin : Solver_intf.PLUGIN_CDCL_T) =
  Make(struct
    include Plugin
    module Term = struct
      type t = Solver_intf.void
      let equal _ _ = assert false
      let hash _ =  assert false
      let pp _ _ = assert false
    end
    let eval _ _ = Solver_intf.Unknown
    let assign _ t = t
    let mcsat = false
    let iter_assignable _ _ _ = ()
  end)
[@@inline][@@specialise]

module Make_mcsat(Plugin : Solver_intf.PLUGIN_MCSAT) =
  Make(struct
    include Plugin
    let mcsat = true
  end)
[@@inline][@@specialise]

module Make_pure_sat(F: Solver_intf.FORMULA) =
  Make(struct
  module Formula = F
  module Term = struct
    type t = Solver_intf.void
    let equal _ _ = true
    let hash _ = 1
    let pp out _ = Format.pp_print_string out "()"
  end
  type t = unit
  type proof = Solver_intf.void
  type level = unit
  let current_level () = ()
  let assume () _ = Solver_intf.Th_sat
  let if_sat () _ = Solver_intf.Th_sat
  let backtrack () _ = ()
  let eval () _ = Solver_intf.Unknown
  let assign () t = t
  let mcsat = false
  let iter_assignable () _ _ = ()
    let mcsat = false
  end)
[@@inline][@@specialise]

