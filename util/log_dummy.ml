(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

(** {1 Logging functions, dummy version}

    This does nothing. *)

let debug_level_ = ref 0
let set_debug l = debug_level_ := l
let get_debug () = !debug_level_

let debugf _ _ _ = ()
let debug _ _ = ()

let set_debug_out _ = ()
