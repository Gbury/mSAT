(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2015 Guillaume Bury
*)

module type S = Backend_intf.S

module type Arg = sig

  type hyp
  type lemma
  type assumption

  val prove_hyp : Format.formatter -> string -> hyp -> unit
  val prove_lemma : Format.formatter -> string -> lemma -> unit
  val prove_assumption : Format.formatter -> string -> assumption -> unit

end

module Make(S : Msat.S)(A : Arg with type hyp := S.clause
                                and type lemma := S.clause
                                and type assumption := S.clause) = struct

  module Atom = S.Atom
  module Clause = S.Clause
  module M = Map.Make(S.Atom)
  module C_tbl = S.Clause.Tbl
  module P = S.Proof

  let name = Clause.name

  let clause_map c =
    let rec aux acc a i =
      if i >= Array.length a then acc
      else begin
        let name = Format.sprintf "A%d" i in
        aux (M.add a.(i) name acc) a (i + 1)
      end
    in
    aux M.empty (Clause.atoms c) 0

  let clause_iter m format fmt clause =
    let aux atom = Format.fprintf fmt format (M.find atom m) in
    Array.iter aux (Clause.atoms clause)

  let elim_duplicate fmt goal hyp _ =
    (** Printing info comment in coq *)
    Format.fprintf fmt
      "(* Eliminating doublons. Goal : %s ; Hyp : %s *)@\n"
      (name goal) (name hyp);
    (** Prove the goal: intro the atoms, then use them with the hyp *)
    let m = clause_map goal in
    Format.fprintf fmt "pose proof @[<hov>(fun %a=>@ %s%a) as %s@].@\n"
      (clause_iter m "%s@ ") goal (name hyp)
      (clause_iter m "@ %s") hyp (name goal)

  let resolution_aux m a h1 h2 fmt () =
    Format.fprintf fmt "%s%a" (name h1)
      (fun fmt -> Array.iter (fun b ->
           if b == a then begin
             Format.fprintf fmt "@ (fun p =>@ %s%a)"
               (name h2) (fun fmt -> (Array.iter (fun c ->
                   if Atom.equal c (Atom.neg a) then
                     Format.fprintf fmt "@ (fun np => np p)"
                   else
                     Format.fprintf fmt "@ %s" (M.find c m)))
                 ) (Clause.atoms h2)
           end else
             Format.fprintf fmt "@ %s" (M.find b m)
         )) (Clause.atoms h1)

  let resolution fmt goal hyp1 hyp2 atom =
    let a = Atom.abs atom in
    let h1, h2 =
      if Array.exists (Atom.equal a) (Clause.atoms hyp1) then hyp1, hyp2
      else (
        assert (Array.exists (Atom.equal a) (Clause.atoms hyp2));
        hyp2, hyp1
      )
    in
    (** Print some debug info *)
    Format.fprintf fmt
      "(* Clausal resolution. Goal : %s ; Hyps : %s, %s *)@\n"
      (name goal) (name h1) (name h2);
    (** Prove the goal: intro the axioms, then perform resolution *)
    if Array.length (Clause.atoms goal) = 0 then (
      let m = M.empty in
      Format.fprintf fmt "exact @[<hov 1>(%a)@].@\n" (resolution_aux m a h1 h2) ();
      false
    ) else (
      let m = clause_map goal in
      Format.fprintf fmt "pose proof @[<hov>(fun %a=>@ %a)@ as %s.@]@\n"
        (clause_iter m "%s@ ") goal (resolution_aux m a h1 h2) () (name goal);
      true
    )

  (* Count uses of hypotheses *)
  let incr_use h c =
    let i = try C_tbl.find h c with Not_found -> 0 in
    C_tbl.add h c (i + 1)

  let decr_use h c =
    let i = C_tbl.find h c - 1 in
    assert (i >= 0);
    let () = C_tbl.add h c i in
    i <= 0

  let clear fmt c =
    Format.fprintf fmt "clear %s." (name c)

  let rec clean_aux fmt = function
    | [] -> ()
    | [x] ->
      Format.fprintf fmt "%a@\n" clear x
    | x :: ((_ :: _) as r) ->
      Format.fprintf fmt "%a@ %a" clear x clean_aux r

  let clean h fmt l =
    match List.filter (decr_use h) l with
    | [] -> ()
    | l' ->
      Format.fprintf fmt "(* Clearing unused clauses *)@\n%a" clean_aux l'

  let prove_node t fmt node =
    let clause = node.P.conclusion in
    match node.P.step with
    | P.Hypothesis _ ->
      A.prove_hyp fmt (name clause) clause
    | P.Assumption ->
      A.prove_assumption fmt (name clause) clause
    | P.Lemma _ ->
      A.prove_lemma fmt (name clause) clause
    | P.Duplicate (p, l) ->
      let c = P.conclusion p in
      let () = elim_duplicate fmt clause c l in
      clean t fmt [c]
    | P.Hyper_res hr ->
      let (p1, p2, a) = P.res_of_hyper_res hr in
      let c1 = P.conclusion p1 in
      let c2 = P.conclusion p2 in
      if resolution fmt clause c1 c2 a then clean t fmt [c1; c2]

  let count_uses p =
    let h = C_tbl.create 128 in
    let aux () node =
      List.iter (fun p' -> incr_use h P.(conclusion p')) (P.parents node.P.step)
    in
    let () = P.fold aux () p in
    h

  (* Here the main idea is to always try and have exactly
     one goal to prove, i.e False. So each  *)
  let pp fmt p =
    let h = count_uses p in
    let aux () node =
      Format.fprintf fmt "%a" (prove_node h) node
    in
    Format.fprintf fmt "(* Coq proof generated by mSAT*)@\n";
    P.fold aux () p
end


module Simple(S : Msat.S)
    (A : Arg with type hyp = S.formula list
              and type lemma := S.lemma
              and type assumption := S.formula) =
  Make(S)(struct
    module P = S.Proof

    (* Some helpers *)
    let lit = S.Atom.formula

    let get_assumption c =
      match S.Clause.atoms_l c with
      | [ x ] -> x
      | _ -> assert false

    let get_lemma c =
      match P.expand (P.prove c) with
      | {P.step=P.Lemma p; _} -> p
      | _ -> assert false

    let prove_hyp fmt name c =
      A.prove_hyp fmt name (List.map lit (S.Clause.atoms_l c))

    let prove_lemma fmt name c =
      A.prove_lemma fmt name (get_lemma c)

    let prove_assumption fmt name c =
      A.prove_assumption fmt name (lit (get_assumption c))

  end)

