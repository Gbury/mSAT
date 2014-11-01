(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Some helpers} *)

(** {2 Time facilities} *)

(** Time elapsed since initialization of the program, and time of start *)
let get_total_time, get_start_time =
  let start = Unix.gettimeofday () in
  (function () ->
    let stop = Unix.gettimeofday () in
    stop -. start),
  (function () -> start)

(** {2 Misc} *)

let clear_line () =
  output_string Pervasives.stdout
    "\r                                                         \r";
  flush Pervasives.stdout


let debug_level_ = ref 0
let set_debug l = debug_level_ := l
let get_debug () = !debug_level_
let need_cleanup = ref false
let debug l format =
  let b = Buffer.create 15 in
  if l <= !debug_level_
  then (
    (if !need_cleanup then clear_line ());
    Printf.bprintf b "%% [%.3f] " (get_total_time ());
    Printf.kbprintf
      (fun b -> print_endline (Buffer.contents b))
      b format)
  else
    Printf.ifprintf b format

let pp_pos pos =
  let open Lexing in
  Printf.sprintf "line %d, column %d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol)

(** {2 Printing utils} *)

let sprintf format =
  let buffer = Buffer.create 64 in
  Printf.kbprintf
    (fun fmt -> Buffer.contents buffer)
    buffer
    format

let fprintf oc format =
  let buffer = Buffer.create 64 in
  Printf.kbprintf
    (fun fmt -> Buffer.output_buffer oc buffer)
    buffer
    format

let printf format = fprintf stdout format
let eprintf format = fprintf stderr format

let on_fmt pp x =
  pp Format.str_formatter x;
  Format.flush_str_formatter ()

let on_buffer pp x =
  let buf = Buffer.create 24 in
  pp buf x;
  Buffer.contents buf

let pp_pair ?(sep=" ") px py buf (x,y) =
  px buf x;
  Buffer.add_string buf sep;
  py buf y

let pp_opt pp buf x = match x with
  | None -> Buffer.add_string buf "None"
  | Some x -> Printf.bprintf buf "Some %a" pp x

(** print a list of items using the printing function *)
let rec pp_list ?(sep=", ") pp_item buf = function
  | x::((y::xs) as l) ->
    pp_item buf x;
    Buffer.add_string buf sep;
    pp_list ~sep pp_item buf l
  | x::[] -> pp_item buf x
  | [] -> ()

(** print an array of items using the printing function *)
let pp_array ?(sep=", ") pp_item buf a =
  for i = 0 to Array.length a - 1 do
    (if i > 0 then Buffer.add_string buf sep);
    pp_item buf a.(i)
  done

(** print an array of items using the printing function *)
let pp_arrayi ?(sep=", ") pp_item buf a =
  for i = 0 to Array.length a - 1 do
    (if i > 0 then Buffer.add_string buf sep);
    pp_item buf i a.(i)
  done
