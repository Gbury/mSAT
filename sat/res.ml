(* Copyright 2014 Guillaume Bury *)

module type S = Res_intf.S

module Make(St : Solver_types.S)(Proof : sig type t end) = struct

    (* Type definitions *)
    type lemma = Proof.t
    type clause = St.clause
    type atom = St.atom
    type int_cl = St.atom list

    type node =
        | Assumption
        | Lemma of lemma
        | Resolution of atom * int_cl * int_cl
        (* lits, c1, c2 with lits the literals used to resolve c1 and c2 *)

    exception Tautology
    exception Resolution_error of string

    (* Proof graph *)
    module H = Hashtbl.Make(struct
        type t = St.atom list
        let hash= Hashtbl.hash
        let equal = List.for_all2 (==)
    end)
    let proof : node H.t = H.create 1007;;

    (* Misc functions *)
    let compare_atoms a b =
        Pervasives.compare St.(a.aid) St.(b.aid)

    let equal_atoms a b = St.(a.aid) = St.(b.aid)


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

    let rec add_res c d =
        add_clause c;
        add_clause d;
        let cl_c = to_list c in
        let cl_d = to_list d in
        let l = List.merge compare_atoms cl_c cl_d in
        let resolved, new_clause = resolve l in
        match resolved with
        | [] -> raise (Resolution_error "No literal to resolve over")
        | [a] ->
                H.add proof new_clause (Resolution (a, cl_c, cl_d));
                new_clause
        | _ -> raise (Resolution_error "Resolved to a tautology")

    and add_clause c =
        let cl = to_list c in
        if is_proved cl then
            ()
        else if not St.(c.learnt) then
            H.add proof cl Assumption
        else begin
            let history = St.(c.cpremise) in
            ()
            (* TODO
            match history with
            | a  :: (_ :: _) as r ->
                    List.fold_left add_res a r
            *)
        end

    (* Print proof graph *)
    let _i = ref 0
    let new_id () = incr _i; "id_" ^ (string_of_int !_i)

    let ids : (bool * string) H.t = H.create 1007;;
    let cl_id c =
        try
            snd (H.find ids c)
        with Not_found ->
            let id = new_id () in
            H.add ids c (false, id);
            id

    let is_drawn c =
        try
            fst (H.find ids c)
        with Not_found ->
            false

    let has_drawn c =
        assert (H.mem ids c);
        let b, id = H.find ids c in
        assert (not b);
        H.replace ids c (true, id)

    let print_atom fmt a =
        Format.fprintf fmt "%s%d" St.(if a.var.pa == a then "" else "-") St.(a.var.vid + 1)

    let rec print_clause fmt = function
        | [] -> Format.fprintf fmt "[]"
        | [a] -> print_atom fmt a
        | a :: (_ :: _) as r -> Format.fprintf fmt "%a \\/ %a" print_atom a print_clause r

    let print_dot_rule f arg fmt cl =
        Format.fprintf fmt "%s [shape=plaintext, label=<<TABLE %s>%a</TABLE>>];@\n"
            (cl_id cl) "BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\">" f arg

    let print_dot_edge c fmt d =
        Format.fprintf fmt "%s -> %s;@\n" (cl_id c) (cl_id d)

    let print_dot_proof fmt cl =
        match H.find proof cl with
        | Assumption ->
                let aux fmt () =
                    Format.fprintf fmt "<TR><TD BGCOLOR=\"LIGHTBLUE\">%a</TD></TR>" print_clause cl
                in
                print_dot_rule aux () fmt cl
        | Lemma _ ->
                let aux fmt () =
                    Format.fprintf fmt "<TR><TD BGCOLOR=\"LIGHTBLUE\">%a</TD></TR><TR><TD>to prove ...</TD></TR>" print_clause cl
                in
                print_dot_rule aux () fmt cl
        | Resolution (r, c, d) ->
                let aux fmt () =
                    Format.fprintf fmt "<TR><TD>%a</TD></TR><TR><TD>%a</TD</TR>"
                        print_clause cl print_atom r
                in
                Format.fprintf fmt "%a%a%a"
                    (print_dot_rule aux ()) cl
                    (print_dot_edge cl) c
                    (print_dot_edge cl) d

    let print_dot fmt cl =
        assert (is_proved cl);
        Format.fprintf fmt "digraph proof {@\n%a@\n}@." print_dot_proof cl

end

