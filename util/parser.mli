(* Copyright 2014 Guillaume Bury *)

exception Syntax_error of int

val parse : string -> int list list
