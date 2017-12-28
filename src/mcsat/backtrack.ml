
module Stack = struct

  type op =
    (* Stack structure *)
    | Nil : op
    | Level : op * int -> op
    (* Undo operations *)
    | Set : 'a ref * 'a * op -> op
    | Call1 : ('a -> unit) * 'a * op -> op
    | Call2 : ('a -> 'b -> unit) * 'a * 'b * op -> op
    | Call3 : ('a -> 'b -> 'c -> unit) * 'a * 'b * 'c * op -> op
    | CallUnit : (unit -> unit) * op -> op

  type t = {
    mutable stack : op;
    mutable last : int;
  }

  type level = int

  let dummy_level = -1

  let create () = {
    stack = Nil;
    last = dummy_level;
  }

  let register_set t ref value = t.stack <- Set(ref, value, t.stack)
  let register_undo t f = t.stack <- CallUnit (f, t.stack)
  let register1 t f x = t.stack <- Call1 (f, x, t.stack)
  let register2 t f x y = t.stack <- Call2 (f, x, y, t.stack)
  let register3 t f x y z = t.stack <- Call3 (f, x, y, z, t.stack)

  let curr = ref 0

  let push t =
    let level = !curr in
    t.stack <- Level (t.stack, level);
    t.last <- level;
    incr curr

  let rec level t =
    match t.stack with
    | Level (_, lvl) -> lvl
    | _ -> push t; level t

  let backtrack t lvl =
    let rec pop = function
      | Nil -> assert false
      | Level (op, level) as current ->
        if level = lvl then begin
          t.stack <- current;
          t.last <- level
        end else
          pop op
      | Set (ref, x, op) -> ref := x; pop op
      | CallUnit (f, op) -> f (); pop op
      | Call1 (f, x, op) -> f x; pop op
      | Call2 (f, x, y, op) -> f x y; pop op
      | Call3 (f, x, y, z, op) -> f x y z; pop op
    in
    pop t.stack

  let pop t = backtrack t (t.last)

end

module Hashtbl(K : Hashtbl.HashedType) = struct

  module H = Hashtbl.Make(K)

  type key = K.t
  type 'a t = {
    tbl : 'a H.t;
    stack : Stack.t;
  }

  let create ?(size=256) stack = {tbl = H.create size; stack; }

  let mem {tbl; _} x = H.mem tbl x
  let find {tbl; _} k = H.find tbl k

  let add t k v =
    Stack.register2 t.stack H.remove t.tbl k;
    H.add t.tbl k v

  let remove t k =
    try
      let v = find t k in
      Stack.register3 t.stack H.add t.tbl k v;
      H.remove t.tbl k
    with Not_found -> ()

  let fold t f acc = H.fold f t.tbl acc

  let iter f t = H.iter f t.tbl
end

