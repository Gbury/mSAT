(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

type extradata = unit
val initial_data : unit -> unit
val file : string ref
val line : int ref
type pos = int
val string_of_pos : int -> string
val cur_pd : unit -> int * unit
type pd = pos * extradata
