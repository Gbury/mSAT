(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module type S = Res_intf.S

module Make(St : Solver_types.S)(Proof : sig type proof end) = struct

  (* Type definitions *)
  type lemma = Proof.proof
  type clause = St.clause
  type atom = St.atom
  type int_cl = clause * St.atom list

  type node =
    | Assumption
    | Lemma of lemma
    | Resolution of atom * int_cl * int_cl
    (* lits, c1, c2 with lits the literals used to resolve c1 and c2 *)

  exception Resolution_error of string

  (* Proof graph *)
  let hash_cl cl =
    Hashtbl.hash (List.map (fun a -> St.(a.aid)) cl)

  let equal_cl cl_c cl_d =
    try
      List.for_all2 (==) cl_c cl_d
    with Invalid_argument _ ->
      false

  module H = Hashtbl.Make(struct
      type t = St.atom list
      let hash = hash_cl
      let equal = equal_cl
    end)
  let proof : node H.t = H.create 1007;;

  (* Misc functions *)
  let equal_atoms a b = St.(a.aid) = St.(b.aid)
  let compare_atoms a b = Pervasives.compare St.(a.aid) St.(b.aid)

  let _c = ref 0
  let fresh_pcl_name () = incr _c; "P" ^ (string_of_int !_c)

  (* Printing functions *)
  let print_atom fmt a =
    Format.fprintf fmt "%s%d" St.(if a.var.pa == a then "" else "-") St.(a.var.vid + 1)

  let rec print_cl fmt = function
    | [] -> Format.fprintf fmt "[]"
    | [a] -> print_atom fmt a
    | a :: ((_ :: _) as r) -> Format.fprintf fmt "%a \\/ %a" print_atom a print_cl r

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

  let to_list c =
    let v = St.(c.atoms) in
    let l = ref [] in
    for i = 0 to Vec.size v - 1 do
      l := (Vec.get v i) :: !l
    done;
    let l, res = resolve (List.sort_uniq compare_atoms !l) in
    if l <> [] then
      raise (Resolution_error "Input cause is a tautology");
    res

  (* Adding new proven clauses *)
  let is_proved c = H.mem proof c
  let is_proven c = is_proved (to_list c)

  let add_res (c, cl_c) (d, cl_d) =
    Log.debug 7 "Resolving clauses :";
    Log.debug 7 "  %a" St.pp_clause c;
    Log.debug 7 "  %a" St.pp_clause d;
    let l = List.merge compare_atoms cl_c cl_d in
    let resolved, new_clause = resolve l in
    match resolved with
    | [] -> raise (Resolution_error "No literal to resolve over")
    | [a] ->
      H.add proof new_clause (Resolution (a, (c, cl_c), (d, cl_d)));
      let new_c = St.make_clause (fresh_pcl_name ()) new_clause (List.length new_clause) true [c; d] in
      Log.debug 5 "New clause : %a" St.pp_clause new_c;
      new_c, new_clause
    | _ -> raise (Resolution_error "Resolved to a tautology")

  let add_clause cl l = (* We assume that all clauses in c.cpremise are already proved ! *)
    match l with
    | a :: ((_ :: _) as r) ->
      let new_c, new_cl = List.fold_left add_res a r in
      if not (equal_cl cl new_cl) then begin
        Log.debug 0 "Expected the following clauses to be equal :";
        Log.debug 0 "expected : %s" (Log.on_fmt print_cl cl);
        Log.debug 0 "found : %a" St.pp_clause new_c;
        assert false
      end
    | _ -> assert false

  let need_clause (c, cl) =
    if is_proved cl then
      []
    else if not St.(c.learnt) then begin
      Log.debug 8 "Adding to hyps : %a" St.pp_clause c;
      H.add proof cl Assumption;
      []
    end else
      St.(c.cpremise)

  let rec do_clause = function
    | [] -> ()
    | c :: r ->
      let cl = to_list c in
      let l = need_clause (c, cl) in
      if l = [] then (* c is either an asusmption, or already proved *)
        do_clause r
      else
        let l' = List.rev_map (fun c -> c, to_list c) l in
        let to_prove = List.filter (fun (_, cl) -> not (is_proved cl)) l' in
        let to_prove = List.rev_map fst to_prove in
        if to_prove = [] then begin
          (* See wether we can prove c right now *)
          add_clause cl l';
          do_clause r
        end else
          (* Or if we have to prove some other clauses first *)
          do_clause (to_prove @ (c :: r))

  let prove c =
    Log.debug 3 "Proving : %a" St.pp_clause c;
    do_clause [c];
    Log.debug 3 "Proved : %a" St.pp_clause c

  let clause_unit a = St.(
      let l = if a.is_true then [a] else [a.neg] in
      make_clause (fresh_pcl_name ()) l 1 true a.var.vpremise
    )

  let rec prove_unsat_cl (c, cl) = match cl with
    | [] -> true
    | a :: r ->
      try
        Log.debug 2 "Eliminating %a in %a" St.pp_atom a St.pp_clause c;
        let d = match St.(a.var.level, a.var.reason) with
          | 0, Some d -> d
          | 0, None -> clause_unit a
          | _ -> raise Exit
        in
        prove d;
        let cl_d = to_list d in
        prove_unsat_cl (add_res (c, cl) (d, cl_d))
      with Exit -> false

  exception Cannot
  let assert_can_prove_unsat c =
    Log.debug 1 "=================== Proof =====================";
    prove c;
    if not (prove_unsat_cl (c, to_list c)) then raise Cannot

  (* Interface exposed *)
  type proof_node = {
    conclusion : clause;
    step : step;
  }
  and proof = unit -> proof_node
  and step =
    | Hypothesis
    | Lemma of lemma
    | Resolution of proof * proof * atom

  let rec return_proof (c, cl) () =
    Log.debug 8 "Returning proof for : %a" St.pp_clause c;
    let st = match H.find proof cl with
      | Assumption -> Hypothesis
      | Lemma l -> Lemma l
      | Resolution (a, cl_c, cl_d) ->
        Resolution (return_proof cl_c, return_proof cl_d, a)
    in
    { conclusion = c; step = st }

  let prove_unsat c =
    assert_can_prove_unsat c;
    return_proof (St.empty_clause, [])

  (* Print proof graph *)
  let _i = ref 0
  let new_id () = incr _i; "id_" ^ (string_of_int !_i)

  let ids : (clause, (bool * string)) Hashtbl.t = Hashtbl.create 1007;;
  let c_id c =
    try
      snd (Hashtbl.find ids c)
    with Not_found ->
      let id = new_id () in
      Hashtbl.add ids c (false, id);
      id

  let clear_ids () =
    Hashtbl.iter (fun c (_, id) -> Hashtbl.replace ids c (false, id)) ids

  let is_drawn c =
    try
      fst (Hashtbl.find ids c)
    with Not_found ->
      false

  let has_drawn c =
    assert (Hashtbl.mem ids c);
    let b, id = Hashtbl.find ids c in
    assert (not b);
    Hashtbl.replace ids c (true, id)

  let print_clause fmt c = print_cl fmt (to_list c)

  let print_dot_rule opt f arg fmt cl =
    Format.fprintf fmt "%s [shape=plaintext, label=<<TABLE %s %s>%a</TABLE>>];@\n"
      (c_id cl) "BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\"" opt f arg

  let print_dot_edge id_c fmt id_d =
    Format.fprintf fmt "%s -> %s;@\n" id_c id_d

  let print_res_atom id fmt a =
      Format.fprintf fmt "%s [label=\"%a\"]" id print_atom a

  let print_res_node concl p1 p2 fmt atom =
      let id = new_id () in
      Format.fprintf fmt "%a;@\n%a%a%a"
      (print_res_atom id) atom
      (print_dot_edge (c_id concl)) id
      (print_dot_edge id) (c_id p1)
      (print_dot_edge id) (c_id p2)

  let rec print_dot_proof fmt p =
    match p.step with
    | Hypothesis ->
      let aux fmt () =
        Format.fprintf fmt "<TR><TD colspan=\"2\">%a</TD></TR><TR><TD>Hypothesis</TD><TD>%s</TD></TR>"
          print_clause p.conclusion St.(p.conclusion.name)
      in
      print_dot_rule "BGCOLOR=\"LIGHTBLUE\"" aux () fmt p.conclusion
    | Lemma _ ->
      let aux fmt () =
        Format.fprintf fmt "<TR><TD colspan=\"2\"BGCOLOR=\"LIGHTBLUE\">%a</TD></TR><TR><TD>Lemma</TD><TD>%s</TD></TR>"
          print_clause p.conclusion St.(p.conclusion.name)
      in
      print_dot_rule "BGCOLOR=\"RED\"" aux () fmt p.conclusion
    | Resolution (proof1, proof2, a) ->
      let aux fmt () =
        Format.fprintf fmt "<TR><TD colspan=\"2\">%a</TD></TR><TR><TD>%s</TD><TD>%s</TD></TR>"
        print_clause p.conclusion
        "Resolution" St.(p.conclusion.name)
      in
      let p1 = proof1 () in
      let p2 = proof2 () in
      Format.fprintf fmt "%a%a%a%a"
        (print_dot_rule "" aux ()) p.conclusion
        (print_res_node p.conclusion p1.conclusion p2.conclusion) a
        print_dot_proof p1
        print_dot_proof p2

  let print_dot fmt proof =
    clear_ids ();
    Format.fprintf fmt "digraph proof {@\n%a@\n}@." print_dot_proof (proof ())

end

