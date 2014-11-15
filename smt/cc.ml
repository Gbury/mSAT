(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module Make(T : Sig.OrderedType) = struct

    module M = Map.Make(T)
    module U = Unionfind.Make(T)

    exception Unsat of (T.t * T.t * (T.t list))

    type t = {
        repr : U.t;
        expl : T.t M.t;
        size : int M.t;
    }

    let empty = {
        repr = U.empty;
        expl = M.empty;
        size = M.empty;
    }

    let map_find m v default = try M.find v m with Not_found -> default
    let find_parent v m = map_find m v v

    let rev_root m root =
        let rec aux m curr next =
            if T.compare curr next = 0 then
                m, curr
            else
                let parent = find_parent next m in
                let m' = M.add next curr (M.remove curr m) in
                aux m' next parent
        in
        aux m root (find_parent root m)

    let rec root m acc curr =
        let parent = find_parent curr m in
        if T.compare curr parent = 0 then
            curr :: acc
        else
            root m (curr :: acc) parent

    let expl t a b =
        let rec aux last = function
            | x :: r, y :: r' when T.compare x y = 0 ->
                aux (Some x) (r, r')
            | l, l' -> begin match last with
                | Some z -> List.rev_append (z :: l) l'
                | None -> List.rev_append l l'
            end
        in
        aux None (root t.expl [] a, root t.expl [] b)

    let add_eq_aux t i j =
        if T.compare (U.find t.repr i) (U.find t.repr j) = 0 then
            t
        else
            let m, old_root_i = rev_root t.expl i in
            let m, old_root_j = rev_root m j in
            let nb_i = map_find t.size old_root_i 0 in
            let nb_j = map_find t.size old_root_j 0 in
            if nb_i < nb_j then {
                repr = t.repr;
                expl = M.add i j m;
                size = M.add j (nb_i + nb_j + 1) t.size; }
            else {
                repr = t.repr;
                expl = M.add j i m;
                size = M.add i (nb_i + nb_j + 1) t.size; }

    let add_eq t i j =
        let t' = add_eq_aux t i j in
        try
            let u' = U.union t.repr i j in
            { t' with repr = u' }
        with U.Unsat (a, b) ->
            raise (Unsat (a, b, expl t' a b))

    let add_neq t i j =
        try
            let u' = U.forbid t.repr i j in
            { t with repr = u' }
        with U.Unsat (a, b) ->
            raise (Unsat (a, b, expl t a b))


end
