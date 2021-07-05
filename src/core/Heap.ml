
module type ELEMENTS = Heap_intf.ELEMENTS

module type S = Heap_intf.S

(* TODO: move to Internal
module A1 = Bigarray.Array1

type int32_arr = (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t
type f64_arr = (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t

let mk_int32_arr n = A1.create Bigarray.Int32 Bigarray.C_layout n
let mk_f64_arr n = A1.create Bigarray.Float64 Bigarray.C_layout n
   *)

let _absent_index = -1

module Make(E : ELEMENTS) = struct
  type t = E.t
  type elt = E.elt

  let[@inline] left i = (i lsl 1) + 1 (* i*2 + 1 *)
  let[@inline] right i = (i + 1) lsl 1 (* (i+1)*2 *)
  let[@inline] parent i = (i - 1) asr 1 (* (i-1) / 2 *)

  (*
  let rec heap_property cmp ({heap=heap} as s) i =
    i >= (Vec.size heap)  ||
      ((i = 0 || not(cmp (Vec. get heap i) (Vec.get heap (parent i))))
       && heap_property cmp s (left i) && heap_property cmp s (right i))

  let heap_property cmp s = heap_property cmp s 1
  *)

  (* [elt] is above or at its expected position. Move it up the heap
     (towards high indices) to restore the heap property *)
  let percolate_up (self:t) (elt : elt) : unit =
    let i = ref (E.elt_idx self elt) in
    let pi = ref (parent !i) in
    while !i <> 0 && E.cmp_elt self elt (Vec.get heap !pi) do
      Vec.set heap !i (Vec.get heap !pi);
      Elt.set_idx (Vec.get heap !i) !i;
      i  := !pi;
      pi := parent !i
    done;
    Vec.set heap !i elt;
    Elt.set_idx elt !i

  let percolate_down {heap} (elt:Elt.t): unit =
    let sz = Vec.size heap in
    let li = ref (left (Elt.idx elt)) in
    let ri = ref (right (Elt.idx elt)) in
    let i = ref (Elt.idx elt) in
    begin
      try
        while !li < sz do
          let child =
            if !ri < sz && Elt.cmp (Vec.get heap !ri) (Vec.get heap !li)
            then !ri
            else !li
          in
          if not (Elt.cmp (Vec.get heap child) elt) then raise Exit;
          Vec.set heap !i (Vec.get heap child);
          Elt.set_idx (Vec.get heap !i) !i;
          i  := child;
          li := left !i;
          ri := right !i
        done;
      with Exit -> ()
    end;
    Vec.set heap !i elt;
    Elt.set_idx elt !i

  let[@inline] in_heap x = Elt.idx x >= 0

  let[@inline] decrease s x = assert (in_heap x); percolate_up s x

  (*
  let increase cmp s n =
    assert (in_heap s n); percolate_down cmp s (V.get s.indices n)
  *)

  let filter s filt =
    let j = ref 0 in
    let lim = Vec.size s.heap in
    for i = 0 to lim - 1 do
      if filt (Vec.get s.heap i) then (
        Vec.set s.heap !j (Vec.get s.heap i);
        Elt.set_idx (Vec.get s.heap i) !j;
        incr j;
      ) else (
        Elt.set_idx (Vec.get s.heap i) _absent_index;
      );
    done;
    Vec.shrink s.heap (lim - !j);
    for i = (lim / 2) - 1 downto 0 do
      percolate_down s (Vec.get s.heap i)
    done

  let size s = Vec.size s.heap

  let is_empty s = Vec.is_empty s.heap

  let clear {heap} =
    Vec.iter (fun e -> Elt.set_idx e _absent_index) heap;
    Vec.clear heap;
    ()

  let insert s elt =
    if not (in_heap elt) then (
      Elt.set_idx elt (Vec.size s.heap);
      Vec.push s.heap elt;
      percolate_up s elt;
    )

  (*
  let update cmp s n =
    assert (heap_property cmp s);
    begin
      if in_heap s n then
        begin
          percolate_up cmp s (Vec.get s.indices n);
          percolate_down cmp s (Vec.get s.indices n)
        end
      else insert cmp s n
    end;
    assert (heap_property cmp s)
  *)

  let remove_min ({heap} as s) =
    match Vec.size heap with
    | 0 -> raise Not_found
    | 1 ->
      let x = Vec.pop heap in
      Elt.set_idx x _absent_index;
      x
    | _ ->
      let x = Vec.get heap 0 in
      let new_hd = Vec.pop heap in (* heap.last() *)
      Vec.set heap 0 new_hd;
      Elt.set_idx x _absent_index;
      Elt.set_idx new_hd 0;
      (* enforce heap property again *)
      if Vec.size heap > 1 then (
        percolate_down s new_hd;
      );
      x

end [@@inline]
