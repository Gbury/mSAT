
module type S = Backend_intf.S

module Make
    (St : Solver_types.S)
    (S : Res.S with type clause = St.clause
                and type lemma = St.proof
                and type atom = St.atom) = struct

  let clause_id c = St.(c.name)

  let node_id n = clause_id S.(n.conclusion)

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
    Format.fprintf fmt "%s [label=\"%a\"];@\n" id St.print_atom a

  let ttify f c = fun fmt () -> f fmt c

  let print_contents fmt n =
    match S.(n.step) with
    | S.Hypothesis ->
      print_dot_node fmt (node_id n) "LIGHTBLUE" S.(n.conclusion) "Hypothesis" "LIGHTBLUE"
        [(fun fmt () -> (Format.fprintf fmt "%s" n.S.conclusion.St.name))];
    | S.Lemma lemma ->
      let rule, f_args, t_args, color = St.proof_debug lemma in
      let color = match color with None -> "YELLOW" | Some c -> c in
      let l = List.map (ttify St.print_atom) f_args @
              List.map (ttify St.print_lit) t_args in
      print_dot_node fmt (node_id n) "LIGHTBLUE" S.(n.conclusion) rule color l
    | S.Resolution (_, _, a) ->
      print_dot_node fmt (node_id n) "GREY" S.(n.conclusion) "Resolution" "GREY"
        [(fun fmt () -> (Format.fprintf fmt "%s" n.S.conclusion.St.name))];
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



