(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

(** {1 Logging functions, real version} *)

let debug_level_ = ref 0
let set_debug l = debug_level_ := l
let get_debug () = !debug_level_

let debug_fmt_ = ref Format.err_formatter

let set_debug_out f = debug_fmt_ := f

let debugf l format k =
  if l <= !debug_level_
  then
    k (Format.kfprintf
        (fun fmt -> Format.fprintf fmt "@]@.")
        !debug_fmt_ format)

let debug l msg = debugf l "%s" (fun k->k msg)
