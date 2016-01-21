(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module type S = Res_intf.S

module Make(L : Log_intf.S)(St : Solver_types.S) = struct

  module St = St

  (* Type definitions *)
  type lemma = St.proof
  type clause = St.clause
  type atom = St.atom
  type int_cl = clause * St.atom list

  exception Insuficient_hyps
  exception Resolution_error of string

  (* Misc functions *)
  let equal_atoms a b = St.(a.aid) = St.(b.aid)
  let compare_atoms a b = Pervasives.compare St.(a.aid) St.(b.aid)

  let merge = List.merge compare_atoms

  let _c = ref 0
  let fresh_pcl_name () = incr _c; "R" ^ (string_of_int !_c)

  (* Printing functions *)
  let rec print_cl fmt = function
    | [] -> Format.fprintf fmt "[]"
    | [a] -> St.print_atom fmt a
    | a :: ((_ :: _) as r) -> Format.fprintf fmt "%a ∨ %a" St.print_atom a print_cl r

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

  (* List.sort_uniq is only since 4.02.0 *)
  let sort_uniq compare l =
    let rec aux = function
      | x :: ((y :: _) as r) -> if compare x y = 0 then aux r else x :: aux r
      | l -> l
    in
    aux (List.sort compare l)

  let to_list c =
    let v = St.(c.atoms) in
    let l = ref [] in
    for i = 0 to Vec.size v - 1 do
      l := (Vec.get v i) :: !l
    done;
    let res = sort_uniq compare_atoms !l in
    let l, _ = resolve res in
    if l <> [] then
      L.debug 3 "Input clause is a tautology";
    res

  (* Printing *)
  let print_clause fmt c = print_cl fmt (to_list c)

  (* Comparison of clauses *)
  let cmp_cl c d =
    let rec aux = function
      | [], [] -> 0
      | a :: r, a' :: r' -> begin match compare_atoms a a' with
          | 0 -> aux (r, r')
          | x -> x
        end
      | _ :: _ , [] -> -1
      | [], _ :: _ -> 1
    in
    aux (c, d)

  let cmp c d =
    cmp_cl (to_list c) (to_list d)

  let prove conclusion =
    assert St.(conclusion.learnt || conclusion.cpremise <> History []);
    conclusion

  let prove_unsat c =
    let l = Vec.to_list c.St.atoms in
    let l = List.map (fun a ->
        match St.(a.var.reason) with
        | St.Bcp Some d -> d
        | _ -> assert false) l
    in
    St.make_clause (fresh_pcl_name ()) [] 0 true (St.History (c :: l))
      (List.fold_left (fun i c -> max i c.St.c_level) 0 l)

  (* Interface exposed *)
  type proof = clause
  and proof_node = {
    conclusion : clause;
    step : step;
  }
  and step =
    | Hypothesis
    | Lemma of lemma
    | Resolution of proof * proof * atom

  let rec chain_res (c, cl) = function
    | d :: r ->
      L.debug 10 " Resolving :";
      L.debug 10 "   - %a" St.pp_clause c;
      L.debug 10 "   - %a" St.pp_clause d;
      let dl = to_list d in
      begin match resolve (merge cl dl) with
      | [ a ], l ->
        begin match r with
          | [] -> (l, c, d, a)
          | _ ->
            let new_clause = St.make_clause (fresh_pcl_name ()) l (List.length l) true
                (St.History [c; d]) (max c.St.c_level d.St.c_level) in
            chain_res (new_clause, l) r
        end
      | _ -> assert false
      end
    | _ -> assert false

  let rec expand conclusion =
    L.debug 5 "Expanding : %a" St.pp_clause conclusion;
    match conclusion.St.cpremise with
    | St.Lemma l ->
      {conclusion; step = Lemma l; }
    | St.History [] ->
      assert (not conclusion.St.learnt);
      { conclusion; step = Hypothesis; }
    | St.History [ c ] ->
      assert (cmp c conclusion = 0);
      expand c
    | St.History ( c :: ([d] as r)) ->
      let (l, c', d', a) = chain_res (c, to_list c) r in
      assert (cmp_cl l (to_list conclusion) = 0);
      { conclusion; step = Resolution (c', d', a); }
    | St.History ( c :: r ) ->
      let (l, c', d', a) = chain_res (c, to_list c) r in
      conclusion.St.cpremise <- St.History [c'; d'];
      assert (cmp_cl l (to_list conclusion) = 0);
      { conclusion; step = Resolution (c', d', a); }

  (* Compute unsat-core *)
  let unsat_core proof =
    let rec aux acc proof =
      let p = expand proof in
      match p.step with
      | Hypothesis | Lemma _ -> p.conclusion :: acc
      | Resolution (proof1, proof2, _) ->
        aux (aux acc proof1) proof2
    in
    sort_uniq cmp (aux [] proof)

  (* Iter on proofs *)
  module H = Hashtbl.Make(struct
      type t = clause
      let hash cl =
        Vec.fold (fun i a -> Hashtbl.hash St.(a.aid, i)) 0 cl.St.atoms
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
          | Resolution (p1, p2, _) ->
            Stack.push (Enter p2) s;
            Stack.push (Enter p1) s
          | _ -> ()
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

