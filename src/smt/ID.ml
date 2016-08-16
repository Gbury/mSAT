(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2016 Guillaume Bury
Copyright 2016 Simon Cruanes
*)

type t = {
  id: int;
  name: string;
}

let make =
  let n = ref 0 in
  fun name ->
    let x = { id= !n; name; } in
    incr n;
    x

let copy {name;_} = make name

let to_string id = id.name

let equal a b = a.id=b.id
let compare a b = Pervasives.compare a.id b.id
let hash a = a.id land max_int
let pp out a = Format.fprintf out "%s/%d" a.name a.id
let pp_name out a = Format.pp_print_string out a.name

module AsKey = struct
  type t_ = t
  type t = t_
  let equal = equal
  let compare = compare
  let hash = hash
end

module Map = Map.Make(AsKey)
module Set = Set.Make(AsKey)
module Tbl = Hashtbl.Make(AsKey)
