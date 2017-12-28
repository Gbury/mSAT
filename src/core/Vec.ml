(**************************************************************************)
(*                                                                        *)
(*                                  Cubicle                               *)
(*             Combining model checking algorithms and SMT solvers        *)
(*                                                                        *)
(*                  Mohamed Iguernelala                                   *)
(*                  Universite Paris-Sud 11                               *)
(*                                                                        *)
(*  Copyright 2011. This file is distributed under the terms of the       *)
(*  Apache Software License version 2.0                                   *)
(*                                                                        *)
(**************************************************************************)

type 'a t = {
  mutable dummy: 'a;
  mutable data : 'a array;
  mutable sz : int;
}

let _size_too_big()=
  failwith "Vec: capacity exceeds maximum array size"

let make capa d =
  if capa > Sys.max_array_length then _size_too_big();
  {data = Array.make capa d; sz = 0; dummy = d}

let[@inline] make_empty d = {data = [||]; sz=0; dummy=d }

let init capa f d =
  if capa > Sys.max_array_length then _size_too_big();
  {data = Array.init capa (fun i -> f i); sz = capa; dummy = d}

let from_array data sz d =
  assert (sz <= Array.length data);
  {data = data; sz = sz; dummy = d}

let from_list l d =
  let a = Array.of_list l in
  from_array a (Array.length a) d

let to_list s =
  let l = ref [] in
  for i = 0 to s.sz - 1 do
    l := s.data.(i) :: !l
  done;
  List.rev !l

let[@inline] clear s = s.sz <- 0

let[@inline] shrink t i =
  assert (i >= 0);
  assert (i<=t.sz);
  t.sz <- i

let[@inline] pop t =
  if t.sz = 0 then invalid_arg "vec.pop";
  t.sz <- t.sz - 1

let[@inline] size t = t.sz

let[@inline] is_empty t = t.sz = 0

let grow_to_exact t new_capa =
  assert (new_capa > Array.length t.data);
  let new_data = Array.make new_capa t.dummy in
  assert (t.sz <= new_capa);
  Array.blit t.data 0 new_data 0 t.sz;
  t.data <- new_data

let grow_to_double_size t =
  if Array.length t.data = Sys.max_array_length then _size_too_big();
  let size = min Sys.max_array_length (2* Array.length t.data + 1) in
  grow_to_exact t size

let grow_to_at_least t new_capa =
  assert (new_capa >= 0);
  if new_capa > Sys.max_array_length then _size_too_big ();
  let data = t.data in
  let capa = ref (max (Array.length data) 1) in
  while !capa < new_capa do
    capa := min (2 * !capa + 1) Sys.max_array_length;
  done;
  if !capa > Array.length data then (
    grow_to_exact t !capa
  )

let[@inline] is_full t = Array.length t.data = t.sz

let[@inline] push t e =
  if is_full t then grow_to_double_size t;
  t.data.(t.sz) <- e;
  t.sz <- t.sz + 1

let[@inline] last t =
  if t.sz = 0 then invalid_arg "vec.last";
  t.data.(t.sz - 1)

let[@inline] pop_last t =
  if t.sz = 0 then invalid_arg "vec.pop_last";
  let x = t.data.(t.sz - 1) in
  t.sz <- t.sz - 1;
  x

let[@inline] get t i =
  if i < 0 || i >= t.sz then invalid_arg "vec.get";
  Array.unsafe_get t.data i

let[@inline] set t i v =
  if i < 0 || i > t.sz then invalid_arg "vec.set";
  if i = t.sz then
    push t v
  else
    Array.unsafe_set t.data i v

let[@inline] copy t =
  let data = Array.copy t.data in
  {t with data; }

let[@inline] move_to t t' =
  t'.data <- Array.copy t.data;
  t'.sz <- t.sz

let remove t e =
  let j = ref 0 in
  while (!j < t.sz && not (t.data.(!j) == e)) do incr j done;
  assert (!j < t.sz);
  for i = !j to t.sz - 2 do t.data.(i) <- t.data.(i+1) done;
  pop t

let[@inline] fast_remove t i =
  assert (i < t.sz);
  t.data.(i) <- t.data.(t.sz - 1);
  t.sz <- t.sz - 1

let filter_in_place f vec =
  let i = ref 0 in
  while !i < size vec do
    if f (get vec !i) then incr i else fast_remove vec !i
  done

let sort t f =
  let sub_arr = Array.sub t.data 0 t.sz in
  Array.fast_sort f sub_arr;
  t.data <- sub_arr

let iter f t =
  for i = 0 to size t - 1 do
    f (Array.unsafe_get t.data i)
  done

let append a b =
  grow_to_at_least a (size a + size b);
  iter (push a) b

let fold f acc t =
  let rec _fold f acc t i =
    if i=t.sz
    then acc
    else (
      let acc' = f acc (Array.unsafe_get t.data i) in
      _fold f acc' t (i+1)
    )
  in _fold f acc t 0

exception ExitVec

let exists p t =
  try
    for i = 0 to t.sz - 1 do
      if p (Array.unsafe_get t.data i) then raise ExitVec
    done;
    false
  with ExitVec -> true

let for_all p t =
  try
    for i = 0 to t.sz - 1 do
      if not (p (Array.unsafe_get t.data i)) then raise ExitVec
    done;
    true
  with ExitVec -> false

let print ?(sep=", ") pp out v =
  let first = ref true in
  iter
    (fun x ->
       if !first then first := false else Format.fprintf out "%s@," sep;
       pp out x)
    v

(*
template<class V, class T>
static inline void remove(V& ts, const T& t)
{
    int j = 0;
    for (; j < ts.size() && ts[j] != t; j++);
    assert(j < ts.size());
    ts[j] = ts.last();
    ts.pop();
}
#endif

template<class V, class T>
static inline bool find(V& ts, const T& t)
{
    int j = 0;
    for (; j < ts.size() && ts[j] != t; j++);
    return j < ts.size();
}

#endif
*)
