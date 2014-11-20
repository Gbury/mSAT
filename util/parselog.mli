(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

val complete : string -> string -> string list
val commit_info : string -> string
val last_commit : unit -> string

type status = Sat | Unsat | Timeout | Spaceout
type pb = { pb_name : string; pb_st : status; pb_time : float; }

val parse_commit : string -> (string, pb) Hashtbl.t
