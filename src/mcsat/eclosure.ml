
module type Key = sig
  type t
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val print : Format.formatter -> t -> unit
end

module type S = sig
  type t
  type var

  exception Unsat of var * var * var list

  val create : Backtrack.Stack.t -> t

  val find : t -> var -> var

  val add_eq : t -> var -> var -> unit
  val add_neq : t -> var -> var -> unit
  val add_tag : t -> var -> var -> unit

  val find_tag : t -> var -> var * (var * var) option

end

module Make(T : Key) = struct

  module M = Map.Make(T)
  module H = Backtrack.Hashtbl(T)

  type var = T.t

  exception Equal of var * var
  exception Same_tag of var * var
  exception Unsat of var * var * var list

  type repr_info = {
    rank : int;
    tag : (T.t * T.t) option;
    forbidden : (var * var) M.t;
  }

  type node =
    | Follow of var
    | Repr of repr_info

  type t = {
    size : int H.t;
    expl : var H.t;
    repr : node H.t;
  }

  let create s = {
    size = H.create s;
    expl = H.create s;
    repr = H.create s;
  }

  (* Union-find algorithm with path compression *)
  let self_repr = Repr { rank = 0; tag = None; forbidden = M.empty }

  let find_hash m i default =
    try H.find m i
    with Not_found -> default

  let rec find_aux m i =
    match find_hash m i self_repr with
    | Repr r -> r, i
    | Follow j ->
      let r, k = find_aux m j in
      H.add m i (Follow k);
      r, k

  let get_repr h x =
    let r, y = find_aux h.repr x in
    y, r

  let tag h x v =
    let r, y = find_aux h.repr x in
    let new_m =
      { r with
        tag = match r.tag with
          | Some (_, v') when not (T.equal v v') -> raise (Equal (x, y))
          | (Some _) as t -> t
          | None -> Some (x, v) }
    in
    H.add h.repr y (Repr new_m)

  let find h x = fst (get_repr h x)

  let find_tag h x =
    let r, y = find_aux h.repr x in
    y, r.tag

  let forbid_aux m x =
    try
      let a, b = M.find x m in
      raise (Equal (a, b))
    with Not_found -> ()

  let link h x mx y my =
    let new_m = {
      rank = if mx.rank = my.rank then mx.rank + 1 else mx.rank;
      tag = (match mx.tag, my.tag with
          | Some (z, t1), Some (w, t2) ->
            if not (T.equal t1 t2) then begin
              Log.debugf 3
                (fun k -> k "Tag shenanigan : %a (%a) <> %a (%a)" 
                     T.print t1 T.print z T.print t2 T.print w);
              raise (Equal (z, w))
            end else Some (z, t1)
          | Some t, None | None, Some t -> Some t
          | None, None -> None);
      forbidden = M.merge (fun _ b1 b2 -> match b1, b2 with
          | Some r, _ | None, Some r -> Some r | _ -> assert false)
          mx.forbidden my.forbidden;}
    in
    let aux m z eq =
      match H.find m z with
      | Repr r ->
        let r' = { r with
                   forbidden = M.add x eq (M.remove y r.forbidden) }
        in
        H.add m z (Repr r')
      | _ -> assert false
    in
    M.iter (aux h.repr) my.forbidden;
    H.add h.repr y (Follow x);
    H.add h.repr x (Repr new_m)

  let union h x y =
    let rx, mx = get_repr h x in
    let ry, my = get_repr h y in
    if T.compare rx ry <> 0 then begin
      forbid_aux mx.forbidden ry;
      forbid_aux my.forbidden rx;
      if mx.rank > my.rank then begin
        link h rx mx ry my
      end else begin
        link h ry my rx mx
      end
    end

  let forbid h x y =
    let rx, mx = get_repr h x in
    let ry, my = get_repr h y in
    if T.compare rx ry = 0 then
      raise (Equal (x, y))
    else match mx.tag, my.tag with
      | Some (a, v), Some (b, v') when T.compare v v' = 0 ->
        raise (Same_tag(a, b))
      | _ ->
        H.add h.repr ry (Repr { my with forbidden = M.add rx (x, y) my.forbidden });
        H.add h.repr rx (Repr { mx with forbidden = M.add ry (x, y) mx.forbidden })

  (* Equivalence closure with explanation output *)
  let find_parent v m = find_hash m v v

  let rec root m acc curr =
    let parent = find_parent curr m in
    if T.compare curr parent = 0 then
      curr :: acc
    else
      root m (curr :: acc) parent

  let rec rev_root m curr =
    let next = find_parent curr m in
    if T.compare curr next = 0 then
      curr
    else begin
      H.remove m curr;
      let res = rev_root m next in
      H.add m next curr;
      res
    end

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
    if T.compare (find t i) (find t j) = 0 then
      ()
    else begin
      let old_root_i = rev_root t.expl i in
      let old_root_j = rev_root t.expl j in
      let nb_i = find_hash t.size old_root_i 0 in
      let nb_j = find_hash t.size old_root_j 0 in
      if nb_i < nb_j then begin
        H.add t.expl i j;
        H.add t.size j (nb_i + nb_j + 1)
      end else begin
        H.add t.expl j i;
        H.add t.size i (nb_i + nb_j + 1)
      end
    end

  (* Functions wrapped to produce explanation in case
   * something went wrong *)
  let add_tag t x v =
    match tag t x v with
    | () -> ()
    | exception Equal (a, b) ->
      raise (Unsat (a, b, expl t a b))

  let add_eq t i j =
    add_eq_aux t i j;
    match union t i j with
    | () -> ()
    | exception Equal (a, b) ->
      raise (Unsat (a, b, expl t a b))

  let add_neq t i j =
    match forbid t i j with
    | () -> ()
    | exception Equal (a, b) ->
      raise (Unsat (a, b, expl t a b))
    | exception Same_tag (_, _) ->
      add_eq_aux t i j;
      let res = expl t i j in
      raise (Unsat (i, j, res))

end
