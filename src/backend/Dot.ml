(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

(** Output interface for the backend *)
module type S = Backend_intf.S

(** Input module for the backend *)
module type Arg = sig

  type atom
  (* Type of atoms *)

  type hyp
  type lemma
  type assumption
  (** Types for hypotheses, lemmas, and assumptions. *)

  val print_atom : Format.formatter -> atom -> unit
  (** Printing function for atoms *)

  val hyp_info : hyp -> string * string option * (Format.formatter -> unit -> unit) list
  val lemma_info : lemma -> string * string option * (Format.formatter -> unit -> unit) list
  val assumption_info : assumption -> string * string option * (Format.formatter -> unit -> unit) list
  (** Functions to return information about hypotheses and lemmas *)

end

module Default(S : Msat.S) = struct
  module Atom = S.Atom
  module Clause = S.Clause

  let print_atom = Atom.pp

  let hyp_info c =
    "hypothesis", Some "LIGHTBLUE",
    [ fun fmt () -> Format.fprintf fmt "%s" @@ Clause.name c]

  let lemma_info c =
    "lemma", Some "BLUE",
    [ fun fmt () -> Format.fprintf fmt "%s" @@ Clause.name c]

  let assumption_info c =
    "assumption", Some "PURPLE",
    [ fun fmt () -> Format.fprintf fmt "%s" @@ Clause.name c]

end

(** Functor to provide dot printing *)
module Make(S : Msat.S)(A : Arg with type atom := S.atom
                                and type hyp := S.clause
                                and type lemma := S.clause
                                and type assumption := S.clause) = struct
  module Atom = S.Atom
  module Clause = S.Clause
  module P = S.Proof

  let node_id n = Clause.name n.P.conclusion
  let proof_id p = node_id (P.expand p)
  let res_nn_id n1 n2 = node_id n1 ^ "_" ^ node_id n2 ^ "_res"
  let res_np_id n1 n2 = node_id n1 ^ "_" ^ proof_id n2 ^ "_res"

  let print_clause fmt c =
    let v = Clause.atoms c in
    if Array.length v = 0 then
      Format.fprintf fmt "⊥"
    else
      let n = Array.length v in
      for i = 0 to n - 1 do
        Format.fprintf fmt "%a" A.print_atom v.(i);
        if i < n - 1 then
          Format.fprintf fmt ", "
      done

  let print_edge fmt i j =
    Format.fprintf fmt "%s -> %s;@\n" j i

  let print_edges fmt n =
    match P.(n.step) with
    | P.Hyper_res {P.hr_steps=[];_} -> assert false (* NOTE: should never happen *)
    | P.Hyper_res {P.hr_init; hr_steps=((_,p0)::_) as l} ->
      print_edge fmt (res_np_id n p0) (proof_id hr_init);
      List.iter
        (fun (_,p2) -> print_edge fmt (res_np_id n p2) (proof_id p2))
        l;
    | _ -> ()

  let table_options fmt color =
    Format.fprintf fmt "BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" BGCOLOR=\"%s\"" color

  let table fmt (c, rule, color, l) =
    Format.fprintf fmt "<TR><TD colspan=\"2\">%a</TD></TR>" print_clause c;
    match l with
    | [] ->
      Format.fprintf fmt "<TR><TD BGCOLOR=\"%s\" colspan=\"2\">%s</TD></TR>" color rule
    | f :: r ->
      Format.fprintf fmt "<TR><TD BGCOLOR=\"%s\" rowspan=\"%d\">%s</TD><TD>%a</TD></TR>"
        color (List.length l) rule f ();
      List.iter (fun f -> Format.fprintf fmt "<TR><TD>%a</TD></TR>" f ()) r

  let print_dot_node fmt id color c rule rule_color l =
    Format.fprintf fmt "%s [shape=plaintext, label=<<TABLE %a>%a</TABLE>>];@\n"
      id table_options color table (c, rule, rule_color, l)

  let print_dot_res_node fmt id a =
    Format.fprintf fmt "%s [label=<%a>];@\n" id A.print_atom a

  let ttify f c = fun fmt () -> f fmt c

  let print_contents fmt n =
    match P.(n.step) with
    (* Leafs of the proof tree *)
    | P.Hypothesis _ ->
      let rule, color, l = A.hyp_info P.(n.conclusion) in
      let color = match color with None -> "LIGHTBLUE" | Some c -> c in
      print_dot_node fmt (node_id n) "LIGHTBLUE" P.(n.conclusion) rule color l
    | P.Assumption ->
      let rule, color, l = A.assumption_info P.(n.conclusion) in
      let color = match color with None -> "LIGHTBLUE" | Some c -> c in
      print_dot_node fmt (node_id n) "LIGHTBLUE" P.(n.conclusion) rule color l
    | P.Lemma _ ->
      let rule, color, l = A.lemma_info P.(n.conclusion) in
      let color = match color with None -> "YELLOW" | Some c -> c in
      print_dot_node fmt (node_id n) "LIGHTBLUE" P.(n.conclusion) rule color l

    (* Tree nodes *)
    | P.Duplicate (p, l) ->
      print_dot_node fmt (node_id n) "GREY" P.(n.conclusion) "Duplicate" "GREY"
        ((fun fmt () -> (Format.fprintf fmt "%s" (node_id n))) ::
         List.map (ttify A.print_atom) l);
      print_edge fmt (node_id n) (node_id (P.expand p))
    | P.Hyper_res {P.hr_steps=l; _} ->
      print_dot_node fmt (node_id n) "GREY" P.(n.conclusion) "Resolution" "GREY"
        [(fun fmt () -> (Format.fprintf fmt "%s" (node_id n)))];
      List.iter
        (fun (a,p2) ->
          print_dot_res_node fmt (res_np_id n p2) a;
          print_edge fmt (node_id n) (res_np_id n p2))
        l

  let print_node fmt n =
    print_contents fmt n;
    print_edges fmt n

  let pp fmt p =
    Format.fprintf fmt "digraph proof {@\n";
    P.fold (fun () -> print_node fmt) () p;
    Format.fprintf fmt "}@."

end

module Simple(S : Msat.S)
    (A : Arg with type atom := S.formula
              and type hyp = S.formula list
              and type lemma := S.lemma
              and type assumption = S.formula) =
  Make(S)(struct
    module Atom = S.Atom
    module Clause = S.Clause
    module P = S.Proof

    (* Some helpers *)
    let lit = Atom.formula

    let get_assumption c =
      match S.Clause.atoms_l c with
      | [ x ] -> x
      | _ -> assert false

    let get_lemma c =
      match P.expand (P.prove c) with
      | {P.step=P.Lemma p;_} -> p
      | _ -> assert false

    (* Actual functions *)
    let print_atom fmt a =
      A.print_atom fmt (lit a)

    let hyp_info c =
      A.hyp_info (List.map lit (S.Clause.atoms_l c))

    let lemma_info c =
      A.lemma_info (get_lemma c)

    let assumption_info c =
      A.assumption_info (lit (get_assumption c))

  end)

