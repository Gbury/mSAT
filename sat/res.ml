(* Copyright 2014 Guillaume Bury *)

module type S = Res_intf.S

module Make(St : Solver_types.S)(Proof : sig type t end) = struct

    (* Type definitions *)
    type lemma = Proof.t
    type clause = St.clause
    type int_cl = St.atom list

    type node =
        | Assumption
        | Lemma of lemma
        | Resolution of int_cl * int_cl

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
        Pervasives.compare St.(a.var.vid) St.(b.var.vid)

    let equal_atoms a b = St.(a.aid) = St.(b.aid)

    let to_list c =
        let v = St.(c.atoms) in
        let l = ref [] in
        for i = 0 to Vec.size v - 1 do
            l := (Vec.get v i) :: !l
        done;
        List.sort_uniq compare_atoms !l

    (* Accesors to the proof graph *)
    let add_hyp c = H.add proof c Assumption
    let add_lemma c l = H.add proof c (Lemma l)

    let is_proved c = H.mem proof c

    (* New resolution node *)
    let resolve l =
        let rec aux resolved acc = function
            | [] -> resolved, acc
            | [a] -> resolved, a :: acc
            | a :: b :: r ->
                    if a == b then
                        aux resolved (a :: acc) r
                    else if St.(a.neg) == b then
                        aux true acc r
                    else
                        aux resolved (a :: acc) (b :: r)
        in
        let b, l' = aux false [] l in
        b, List.sort compare_atoms l'

    let merge c d =
        let l = List.merge compare_atoms c d in
        let b, l' = resolve l in
        if not b then
            raise (Resolution_error "No literal to resolve over");
        l'

    let add_res c d =
        if not (is_proved c) || not (is_proved d) then
            raise (Resolution_error "Unproven clause");
        let new_clause = merge c d in
        H.add proof new_clause (Resolution (c, d));
        new_clause

    (* Wrappers *)
    let proven c = is_proved (to_list c)
    let add_assumption c = add_hyp (to_list c)
    let add_th_lemma c l = add_lemma (to_list c) l

    let add_clause c history =
        assert (List.length history > 1);
        let l = List.map to_list history in
        let res = List.fold_left add_res (List.hd l) (List.tl l) in
        if not (List.for_all2 equal_atoms (to_list c) res) then
            raise (Resolution_error "Clause cannot be derived from history");
        ()

end
