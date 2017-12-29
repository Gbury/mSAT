(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module type S = Res_intf.S

module Make(St : Solver_types.S) = struct

  module St = St

  (* Type definitions *)
  type lemma = St.proof
  type clause = St.clause
  type atom = St.atom

  exception Insuficient_hyps
  exception Resolution_error of string

  (* Log levels *)
  let error = 1
  let warn = 3
  let info = 10
  let debug = 80

  let equal_atoms a b = St.(a.aid) = St.(b.aid)
  let compare_atoms a b = Pervasives.compare St.(a.aid) St.(b.aid)

  let print_clause = St.Clause.pp

  let merge = List.merge compare_atoms

  let _c = ref 0
  let fresh_pcl_name () = incr _c; "R" ^ (string_of_int !_c)

  (* Compute resolution of 2 clauses *)
  let resolve l =
    let rec aux resolved acc = function
      | [] -> resolved, acc
      | [a] -> resolved, a :: acc
      | a :: b :: r ->
        if equal_atoms a b then
          aux resolved (a :: acc) r
        else if equal_atoms St.(a.neg) b then
          aux (St.(a.var.pa) :: resolved) acc r
        else
          aux resolved (a :: acc) (b :: r)
    in
    let resolved, new_clause = aux [] [] l in
    resolved, List.rev new_clause

  (* Compute the set of doublons of a clause *)
  let list c = List.sort St.Atom.compare (Array.to_list St.(c.atoms))

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
      Log.debug warn "Input clause has redundancies";
    if conflicts <> [] then
      Log.debug warn "Input clause is a tautology";
    cl

  (*
  let pp_cl fmt l =
    let rec aux fmt = function
      | [] -> ()
      | a :: r ->
        Format.fprintf fmt "%a@,%a" St.pp_atom a aux r
    in
    Format.fprintf fmt "@[<v>%a@]" aux l
  *)

  (* Comparison of clauses *)
  let cmp_cl c d =
    let rec aux = function
      | [], [] -> 0
      | a :: r, a' :: r' ->
        begin match compare_atoms a a' with
          | 0 -> aux (r, r')
          | x -> x
        end
      | _ :: _ , [] -> -1
      | [], _ :: _ -> 1
    in
    aux (c, d)

  let prove conclusion =
    assert St.(conclusion.cpremise <> History []);
    conclusion

  let rec set_atom_proof a =
    let aux acc b =
      if equal_atoms a.St.neg b then acc
      else set_atom_proof b :: acc
    in
    assert St.(a.var.v_level >= 0);
    match St.(a.var.reason) with
    | Some St.Bcp c ->
      Log.debugf debug (fun k->k "Analysing: @[%a@ %a@]" St.Atom.debug a St.Clause.debug c);
      if Array.length c.St.atoms = 1 then (
        Log.debugf debug (fun k -> k "Old reason: @[%a@]" St.Atom.debug a);
        c
      ) else (
        assert (a.St.neg.St.is_true);
        let r = St.History (c :: (Array.fold_left aux [] c.St.atoms)) in
        let c' = St.Clause.make [a.St.neg] r in
        a.St.var.St.reason <- Some St.(Bcp c');
        Log.debugf debug
          (fun k -> k "New reason: @[%a@ %a@]" St.Atom.debug a St.Clause.debug c');
        c'
      )
    | _ ->
      Log.debugf error (fun k -> k "Error while proving atom %a" St.Atom.debug a);
      raise (Resolution_error "Cannot prove atom")

  let prove_unsat conflict =
    if Array.length conflict.St.atoms = 0 then conflict
    else (
      Log.debugf info (fun k -> k "Proving unsat from: @[%a@]" St.Clause.debug conflict);
      let l = Array.fold_left (fun acc a -> set_atom_proof a :: acc) [] conflict.St.atoms in
      let res = St.Clause.make [] (St.History (conflict :: l)) in
      Log.debugf info (fun k -> k "Proof found: @[%a@]" St.Clause.debug res);
      res
    )

  let prove_atom a =
    if St.(a.is_true && a.var.v_level = 0) then
      Some (set_atom_proof a)
    else
      None

  (* Interface exposed *)
  type proof = clause
  and proof_node = {
    conclusion : clause;
    step : step;
  }
  and step =
    | Hypothesis
    | Assumption
    | Lemma of lemma
    | Duplicate of proof * atom list
    | Resolution of proof * proof * atom

  let rec chain_res (c, cl) = function
    | d :: r ->
      Log.debugf debug
        (fun k -> k "  Resolving clauses : @[%a@\n%a@]" St.Clause.debug c St.Clause.debug d);
      let dl = to_list d in
      begin match resolve (merge cl dl) with
        | [ a ], l ->
          begin match r with
            | [] -> (l, c, d, a)
            | _ ->
              let new_clause = St.Clause.make l (St.History [c; d]) in
              chain_res (new_clause, l) r
          end
        | _ ->
          Log.debugf error
            (fun k -> k "While resolving clauses:@[<hov>%a@\n%a@]" St.Clause.debug c St.Clause.debug d);
          raise (Resolution_error "Clause mismatch")
      end
    | _ ->
      raise (Resolution_error "Bad history")

  let expand conclusion =
    Log.debugf debug (fun k -> k "Expanding : @[%a@]" St.Clause.debug conclusion);
    match conclusion.St.cpremise with
    | St.Lemma l ->
      {conclusion; step = Lemma l; }
    | St.Hyp ->
      { conclusion; step = Hypothesis; }
    | St.Local ->
      { conclusion; step = Assumption; }
    | St.History [] ->
      Log.debugf error (fun k -> k "Empty history for clause: %a" St.Clause.debug conclusion);
      raise (Resolution_error "Empty history")
    | St.History [ c ] ->
      let duplicates, res = analyze (list c) in
      assert (cmp_cl res (list conclusion) = 0);
      { conclusion; step = Duplicate (c, List.concat duplicates) }
    | St.History ( c :: ([_] as r)) ->
      let (l, c', d', a) = chain_res (c, to_list c) r in
      assert (cmp_cl l (to_list conclusion) = 0);
      { conclusion; step = Resolution (c', d', a); }
    | St.History ( c :: r ) ->
      let (l, c', d', a) = chain_res (c, to_list c) r in
      conclusion.St.cpremise <- St.History [c'; d'];
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
        if not c.St.visited then (
          c.St.visited <- true;
          match c.St.cpremise with
          | St.Hyp | St.Local | St.Lemma _ -> aux (c :: res) acc r
          | St.History h ->
            let l = List.fold_left (fun acc c ->
                if not c.St.visited then c :: acc else acc) r h in
            aux res (c :: acc) l
        ) else (
          aux res acc r
        )
    in
    let res, tmp = aux [] [] [proof] in
    List.iter (fun c -> c.St.visited <- false) res;
    List.iter (fun c -> c.St.visited <- false) tmp;
    res

  (* Iter on proofs *)
  module H = Hashtbl.Make(struct
      type t = clause
      let hash cl =
        Array.fold_left (fun i a -> Hashtbl.hash St.(a.aid, i)) 0 cl.St.atoms
      let equal = (==)
    end)

  type task =
    | Enter of proof
    | Leaving of proof

  let spop s = try Some (Stack.pop s) with Stack.Empty -> None

  let rec fold_aux s h f acc =
    match spop s with
    | None -> acc
    | Some (Leaving c) ->
      H.add h c true;
      fold_aux s h f (f acc (expand c))
    | Some (Enter c) ->
      if not (H.mem h c) then begin
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
    let h = H.create 42 in
    let s = Stack.create () in
    Stack.push (Enter p) s;
    fold_aux s h f acc

  let check p = fold (fun () _ -> ()) () p

end

