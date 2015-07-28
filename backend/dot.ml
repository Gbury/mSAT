
module type S = Backend_intf.S

module type Arg = sig
  type atom
  type clause
  type lemma

  val clause_name : clause -> string
  val print_atom : Format.formatter -> atom -> unit
  val lemma_info : lemma -> string * string option * (Format.formatter -> unit -> unit) list
end

module Make(S : Res.S)(A : Arg with type atom := S.atom and type clause := S.clause and type lemma := S.lemma) = struct

  let node_id n = A.clause_name S.(n.conclusion)

  let res_node_id n = (node_id n) ^ "_res"

  let proof_id p = node_id (S.expand p)

  let print_edge fmt i j =
    Format.fprintf fmt "%s -> %s;@\n" i j

  let print_edges fmt n =
    match S.(n.step) with
    | S.Resolution (p1, p2, _) ->
      print_edge fmt (res_node_id n) (proof_id p1);
      print_edge fmt (res_node_id n) (proof_id p2)
    | _ -> ()

  let table_options fmt color =
    Format.fprintf fmt "BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" BGCOLOR=\"%s\"" color

  let table fmt (c, rule, color, l) =
    Format.fprintf fmt "<TR><TD colspan=\"2\">%a</TD></TR>" S.print_clause c;
    match l with
    | [] ->
      Format.fprintf fmt "<TR><TD BGCOLOR=\"%s\" colspan=\"2\">%s</TD></TR>" color rule
    | f :: r ->
      Format.fprintf fmt "<TR><TD BGCOLOR=\"%s\" rowspan=\"%d\">%s</TD><TD>%a</TD></TR>"
        color (List.length l) rule f ();
      List.iter (fun f -> Format.fprintf fmt "<TR><TD></TD><TD>%a</TD></TR>" f ()) r

  let print_dot_node fmt id color c rule rule_color l =
    Format.fprintf fmt "%s [shape=plaintext, label=<<TABLE %a>%a</TABLE>>];@\n"
      id table_options color table (c, rule, rule_color, l)

  let print_dot_res_node fmt id a =
    Format.fprintf fmt "%s [label=\"%a\"];@\n" id A.print_atom a

  let ttify f c = fun fmt () -> f fmt c

  let print_contents fmt n =
    match S.(n.step) with
    | S.Hypothesis ->
      print_dot_node fmt (node_id n) "LIGHTBLUE" S.(n.conclusion) "Hypothesis" "LIGHTBLUE"
        [(fun fmt () -> (Format.fprintf fmt "%s" (A.clause_name n.S.conclusion)))];
    | S.Lemma lemma ->
      let rule, color, l = A.lemma_info lemma in
      let color = match color with None -> "YELLOW" | Some c -> c in
      print_dot_node fmt (node_id n) "LIGHTBLUE" S.(n.conclusion) rule color l
    | S.Resolution (_, _, a) ->
      print_dot_node fmt (node_id n) "GREY" S.(n.conclusion) "Resolution" "GREY"
        [(fun fmt () -> (Format.fprintf fmt "%s" (A.clause_name n.S.conclusion)))];
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



