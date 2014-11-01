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

(** time elapsed since start of program *)
val get_total_time : unit -> float

(** time at which the program started *)
val get_start_time : unit -> float

(** {2 Misc} *)

val set_debug : int -> unit     (** Set debug level *)
val get_debug : unit -> int     (** Current debug level *)
val need_cleanup : bool ref     (** Cleanup line before printing? *)

val debug : int -> ('a, Buffer.t, unit, unit) format4 -> 'a
  (** debug message *)

val pp_pos : Lexing.position -> string

(** {2 Printing utils} *)

(** print into a string *)
val sprintf : ('a, Buffer.t, unit, string) format4 -> 'a

val fprintf : out_channel -> ('a, Buffer.t, unit, unit) format4 -> 'a

val printf : ('a, Buffer.t, unit, unit) format4 -> 'a
val eprintf : ('a, Buffer.t, unit, unit) format4 -> 'a
val on_fmt : (Format.formatter -> 'a -> 'b) -> 'a -> string
val on_buffer : (Buffer.t -> 'a -> unit) -> 'a -> string

val pp_pair: ?sep:string -> (Buffer.t -> 'a -> unit) ->
              (Buffer.t -> 'b -> unit) -> Buffer.t -> ('a * 'b) -> unit

val pp_opt : (Buffer.t -> 'a -> unit) -> Buffer.t -> 'a option -> unit

(** print a list of items using the printing function *)
val pp_list: ?sep:string -> (Buffer.t -> 'a -> unit)
          -> Buffer.t -> 'a list -> unit

(** print an array of items with printing function *)
val pp_array: ?sep:string -> (Buffer.t -> 'a -> unit)
          -> Buffer.t -> 'a array -> unit

(** print an array, giving the printing function both index and item *)
val pp_arrayi: ?sep:string -> (Buffer.t -> int -> 'a -> unit)
          -> Buffer.t -> 'a array -> unit
