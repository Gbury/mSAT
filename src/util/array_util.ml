
(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

(** {1 Utils for Arrays} *)

let exists p a =
  let n = Array.length a in
  let rec loop i =
    if i = n then false
    else if p (Array.unsafe_get a i) then true
    else loop (succ i) in
  loop 0

let for_all p a =
  let n = Array.length a in
  let rec loop i =
    if i = n then true
    else if p (Array.unsafe_get a i) then loop (succ i)
    else false in
  loop 0
