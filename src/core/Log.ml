(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

(** {1 Logging functions, real version} *)

let enabled = true (* NOTE: change here for 0-overhead *)

let debug_level_ = ref 0
let set_debug l = debug_level_ := l
let get_debug () = !debug_level_

let debug_fmt_ = ref Format.err_formatter

let set_debug_out f = debug_fmt_ := f

(* does the printing, inconditionally *)
let debug_real_ l k =
  k (fun fmt ->
    Format.fprintf !debug_fmt_ "@[<2>@{<Blue>[%d|%.3f]@}@ "
      l (Sys.time());
    Format.kfprintf
      (fun fmt -> Format.fprintf fmt "@]@.")
      !debug_fmt_ fmt)

let[@inline] debugf l k =
  if enabled && l <= !debug_level_ then (
    debug_real_ l k;
  )

let[@inline] debug l msg = debugf l (fun k->k "%s" msg)
