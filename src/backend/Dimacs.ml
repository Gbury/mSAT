(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

open Msat

module type S = sig

  type clause

  val export :
    Format.formatter ->
    hyps:clause Vec.t ->
    history:clause Vec.t ->
    local:clause Vec.t ->
    unit
  (** Export the given clause vectors to the dimacs format.
      The arguments should be transmitted directly from the corresponding
      function of the {Internal} module. *)

  val export_icnf :
    Format.formatter ->
    hyps:clause Vec.t ->
    history:clause Vec.t ->
    local:clause Vec.t ->
    unit
  (** Export the given clause vectors to the dimacs format.
      The arguments should be transmitted directly from the corresponding
      function of the {Internal} module. *)

end

module Make(St : Solver_types.S)(Dummy: sig end) = struct

  (* Dimacs & iCNF export *)
  let export_vec name fmt vec =
    Format.fprintf fmt "c %s@,%a@," name (Vec.print ~sep:"" St.pp_dimacs) vec

  let export_assumption fmt vec =
    Format.fprintf fmt "c Local assumptions@,a %a@," St.pp_dimacs vec

  let export_icnf_aux r name map_filter fmt vec =
    let aux fmt _ =
      for i = !r to (Vec.size vec) - 1 do
        let x = Vec.get vec i in
        match map_filter x with
        | None -> ()
        | Some _ -> Format.fprintf fmt "%a@," St.pp_dimacs (Vec.get vec i)
      done;
      r := Vec.size vec
    in
    Format.fprintf fmt "c %s@,%a" name aux vec

  let map_filter_learnt c =
    match c.St.cpremise with
    | St.Hyp | St.Local -> assert false
    | St.Lemma _ -> Some c
    | St.History l ->
      begin match l with
        | [] -> assert false
        | d :: _ ->
          begin match d.St.cpremise with
            | St.Lemma _ -> Some d
            | St.Hyp | St.Local | St.History _ -> None
          end
      end

  let filter_vec learnt =
    let lemmas = Vec.make (Vec.size learnt) St.dummy_clause in
    Vec.iter (fun c ->
        match map_filter_learnt c with
        | None -> ()
        | Some d -> Vec.push lemmas d
      ) learnt;
    lemmas

  let export fmt ~hyps ~history ~local =
    assert (Vec.for_all (function
        | { St.cpremise = St.Hyp; _} -> true | _ -> false
      ) hyps);
    (* Learnt clauses, then filtered to only keep only
       the theory lemmas; all other learnt clauses should be logical
       consequences of the rest. *)
    let lemmas = filter_vec history in
    (* Local assertions *)
    assert (Vec.for_all (function
        | { St.cpremise = St.Local; _} -> true | _ -> false
      ) local);
    (* Number of atoms and clauses *)
    let n = St.nb_elt () in
    let m = Vec.size local + Vec.size hyps + Vec.size lemmas in
    Format.fprintf fmt
      "@[<v>p cnf %d %d@,%a%a%a@]@." n m
      (export_vec "Local assumptions") local
      (export_vec "Hypotheses") hyps
      (export_vec "Lemmas") lemmas

  (* Refs to remember what portion of a problem has been printed *)
  let icnf_hyp = ref 0
  let icnf_lemmas = ref 0

  let export_icnf fmt ~hyps ~history ~local =
    assert (Vec.for_all (function
        | { St.cpremise = St.Hyp; _} -> true | _ -> false
      ) hyps);
    let lemmas = history in
    (* Local assertions *)
    let l = List.map (function
        | {St.cpremise = St.Local; atoms = [| a |];_ } -> a
        | _ -> assert false) (Vec.to_list local) in
    let local = St.make_clause "local (tmp)" l St.Local in
    (* Number of atoms and clauses *)
    Format.fprintf fmt
      "@[<v>%s@,%a%a%a@]@."
      (if !icnf_hyp = 0 && !icnf_lemmas = 0 then "p inccnf" else "")
      (export_icnf_aux icnf_hyp "Hypotheses" (fun x -> Some x)) hyps
      (export_icnf_aux icnf_lemmas "Lemmas" map_filter_learnt) lemmas
      export_assumption local

end

