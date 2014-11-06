
(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

(** {1 Sparse vector, filled with default value} *)

let _size_too_big()=
  failwith "Sparse_vec: capacity exceeds maximum array size"

type 'a t = { default : 'a; mutable data : 'a array; mutable sz : int }

let make sz default =
  if sz > Sys.max_array_length then _size_too_big();
  { default; sz; data=Array.make sz default; }

let init sz f default =
  if sz > Sys.max_array_length then _size_too_big();
  {data = Array.init sz (fun i -> f i); sz ; default}

let length {sz} = sz

let grow_to t new_capa =
  let data = t.data in
  let capa = Array.length data in
  t.data <- Array.init new_capa (fun i -> if i < capa then data.(i) else t.default)

let grow_to_double_size t =
  if Array.length t.data = Sys.max_array_length then _size_too_big();
  let size = min Sys.max_array_length (2* Array.length t.data) in
  grow_to t size

let rec grow_to_by_double t new_capa =
  if new_capa > Sys.max_array_length then _size_too_big ();
  let data = t.data in
  let capa = ref (Array.length data + 1) in
  while !capa < new_capa do
    capa := min (2 * !capa) Sys.max_array_length;
  done;
  grow_to t !capa

let resize v len =
  assert (len >= 0);
  if len <= v.sz
  then v.sz <- len
  else (
    grow_to_by_double v len;
    v.sz <- len
  )

let incr v = resize v (v.sz + 1)

let decr v =
  if v.sz = 0 then invalid_arg "Sparse_vec.decr";
  resize v (v.sz - 1)

let is_empty {sz} = sz=0

let clear v = v.sz <- 0

let get t i =
  if i < 0 || i >= t.sz then invalid_arg "Sparse_vec.get";
  Array.unsafe_get t.data i

let set t i v =
  if i < 0 || i >= t.sz then invalid_arg "Sparse_vec.set";
  t.sz <- max t.sz i;
  Array.unsafe_set t.data i v

