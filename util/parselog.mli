(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

exception Empty_raw of string
exception Unknown_status of string list

val complete : string -> string -> string list
val commit_info : string -> string

type status = Sat | Unsat | Timeout | Spaceout
type pb = { pb_name : string; pb_st : status; pb_time : float; }

val parse_raw : string -> pb
val parse_commit : string -> (string, pb) Hashtbl.t
