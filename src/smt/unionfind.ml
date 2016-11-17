(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

(* Union-find Module *)
module Make(T : OrderedType) = struct
  exception Unsat of T.t * T.t

  type var = T.t
  module M = Map.Make(T)

  (* TODO: better treatment of inequalities *)

  type t = {
    rank : int M.t;
    forbid : ((var * var) list);
    mutable parent : var M.t;
  }

  let empty = {
    rank = M.empty;
    forbid = [];
    parent = M.empty;
  }

  let find_map m i default =
    try
      M.find i m
    with Not_found ->
      default

  let rec find_aux f i =
    let fi = find_map f i i in
    if fi = i then
      (f, i)
    else
      let f, r = find_aux f fi in
      let f = M.add i r f in
      (f, r)

  let find h x =
    let f, cx = find_aux h.parent x in
    h.parent <- f;
    cx

  (* Highly inefficient treatment of inequalities *)
  let possible h =
    let aux (a, b) =
      let ca = find h a in
      let cb = find h b in
      if T.compare ca cb = 0 then
        raise (Unsat (a, b))
    in
    List.iter aux h.forbid;
    h

  let union_aux h x y =
    let cx = find h x in
    let cy = find h y in
    if cx != cy then begin
      let rx = find_map h.rank cx 0 in
      let ry = find_map h.rank cy 0 in
      if rx > ry then
        { h with parent = M.add cy cx h.parent }
      else if ry > rx then
        { h with parent = M.add cx cy h.parent }
      else
        { rank = M.add cx (rx + 1) h.rank;
          parent = M.add cy cx h.parent;
          forbid = h.forbid; }
    end else
      h

  let union h x y = possible (union_aux h x y)

  let forbid h x y =
    let cx = find h x in
    let cy = find h y in
    if cx = cy then
      raise (Unsat (x, y))
    else
      { h with forbid = (x, y) :: h.forbid }
end
