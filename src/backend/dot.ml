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
  type lemma
  (** Types *)

  val print_atom : Format.formatter -> atom -> unit
  val lemma_info : lemma -> string * string option * (Format.formatter -> unit -> unit) list
  (** Printing functions for atoms and lemmas. *)

end

(** Functor to provide dot printing *)
module Make(S : Res.S)(A : Arg with type atom := S.atom and type lemma := S.lemma) = struct

  let node_id n = n.S.conclusion.S.St.name

  let res_node_id n = (node_id n) ^ "_res"

  let proof_id p = node_id (S.expand p)

  let print_clause fmt c =
    let v = c.S.St.atoms in
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
    Format.fprintf fmt "%s <- %s;@\n" i j

  let print_edges fmt n =
    match S.(n.step) with
    | S.Resolution (p1, p2, _) ->
      print_edge fmt (res_node_id n) (proof_id p1);
      print_edge fmt (res_node_id n) (proof_id p2)
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
    match S.(n.step) with
    | S.Hypothesis ->
      print_dot_node fmt (node_id n) "LIGHTBLUE" S.(n.conclusion) "Hypothesis" "LIGHTBLUE"
        [(fun fmt () -> (Format.fprintf fmt "%s" (node_id n)))];
    | S.Assumption ->
      print_dot_node fmt (node_id n) "LIGHTBLUE" S.(n.conclusion) "Assumption" "LIGHTBLUE"
        [(fun fmt () -> (Format.fprintf fmt "%s" (node_id n)))];
    | S.Lemma lemma ->
      let rule, color, l = A.lemma_info lemma in
      let color = match color with None -> "YELLOW" | Some c -> c in
      print_dot_node fmt (node_id n) "LIGHTBLUE" S.(n.conclusion) rule color l
    | S.Duplicate (p, l) ->
      print_dot_node fmt (node_id n) "GREY" S.(n.conclusion) "Duplicate" "GREY"
        ((fun fmt () -> (Format.fprintf fmt "%s" (node_id n))) ::
        List.map (ttify A.print_atom) l);
      print_edge fmt (node_id n) (node_id (S.expand p))
    | S.Resolution (_, _, a) ->
      print_dot_node fmt (node_id n) "GREY" S.(n.conclusion) "Resolution" "GREY"
        [(fun fmt () -> (Format.fprintf fmt "%s" (node_id n)))];
      print_dot_res_node fmt (res_node_id n) a;
      print_edge fmt (node_id n) (res_node_id n)

  let print_node fmt n =
    print_contents fmt n;
    print_edges fmt n

  let print fmt p =
    Format.fprintf fmt "digraph proof {@\n";
    S.fold (fun () -> print_node fmt) () p;
    Format.fprintf fmt "}@."

end

module Simple(S : Res.S)(A : Arg with type atom := S.St.formula and type lemma := S.lemma) =
  Make(S)(struct
    let lemma_info = A.lemma_info
    let print_atom fmt a = A.print_atom fmt a.S.St.lit
  end)

