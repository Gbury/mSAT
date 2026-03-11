(* Unit tests for Msat.Heap *)

(* A simple RANKED element: a mutable record with a weight and heap index. *)
module Elt = struct
  type t = {
    weight: int;
    mutable idx: int;
  }

  let make w = { weight = w; idx = -1 }
  let idx e = e.idx
  let set_idx e i = e.idx <- i

  (* max-heap: higher weight = higher priority *)
  let cmp a b = a.weight > b.weight
end

module H = Msat.Heap.Make (Elt)

(* helpers *)
let insert h w =
  let e = Elt.make w in
  H.insert h e;
  e

let drain h =
  let acc = ref [] in
  (try
     while true do
       acc := (H.remove_min h).Elt.weight :: !acc
     done
   with Not_found -> ());
  List.rev !acc

(* Test: filter keeps the right elements and the size is correct.
   Bug: Vec.shrink was called with (lim - kept) instead of (kept),
   so size was under-counted and elements were silently lost. *)
let test_filter_size () =
  let h = H.create () in
  let _e1 = insert h 10 in
  let _e2 = insert h 20 in
  let _e3 = insert h 30 in
  let _e4 = insert h 40 in
  let _e5 = insert h 50 in
  assert (H.size h = 5);
  (* keep only elements with weight >= 30 → should keep 3 *)
  H.filter h (fun e -> e.Elt.weight >= 30);
  let sz = H.size h in
  if sz <> 3 then
    Printf.printf "FAIL test_filter_size: expected size 3, got %d\n%!" sz
  else
    Printf.printf "PASS test_filter_size\n%!"

(* Test: elements removed by filter are marked as not-in-heap (idx = -1),
   and elements kept are still in-heap (idx >= 0). *)
let test_filter_membership () =
  let h = H.create () in
  let e1 = insert h 10 in
  let e2 = insert h 20 in
  let e3 = insert h 30 in
  H.filter h (fun e -> e.Elt.weight >= 20);
  let ok =
    (not (H.in_heap e1))
    && H.in_heap e2
    && H.in_heap e3
  in
  if not ok then
    Printf.printf "FAIL test_filter_membership: in_heap flags wrong\n%!"
  else
    Printf.printf "PASS test_filter_membership\n%!"

(* Test: after filter, remove_min yields exactly the kept elements. *)
let test_filter_contents () =
  let h = H.create () in
  List.iter (fun w -> ignore (insert h w)) [ 5; 15; 25; 35; 45 ];
  (* keep odd-indexed weights: 15, 35 — i.e. weight mod 10 = 5 but > 10 *)
  H.filter h (fun e -> e.Elt.weight > 10 && e.Elt.weight mod 20 = 15);
  let got = drain h in
  (* Elt.cmp = (>), so this is a max-heap: remove_min returns largest first *)
  let expected = [ 35; 15 ] in
  if got <> expected then
    Printf.printf "FAIL test_filter_contents: expected %s, got %s\n%!"
      (String.concat "," (List.map string_of_int expected))
      (String.concat "," (List.map string_of_int got))
  else
    Printf.printf "PASS test_filter_contents\n%!"

let () =
  test_filter_size ();
  test_filter_membership ();
  test_filter_contents ()
