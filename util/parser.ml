(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

exception Syntax_error of int

type line =
  | Empty
  | Comment
  | Pcnf of int * int
  | Clause of int list

let rec _read_word s acc i len =
  assert (len>0);
  if i+len=String.length s
  then String.sub s i len :: acc
  else match s.[i+len] with
    | ' ' | '\t' ->
      let acc = String.sub s i len :: acc in
      _skip_space s acc (i+len+1)
    | _ -> _read_word s acc i (len+1)
and _skip_space s acc i =
  if i=String.length s
  then acc
  else match s.[i] with
    | ' ' | '\t' -> _skip_space s acc (i+1)
    | _ -> _read_word s acc i 1

let ssplit s = List.rev (_skip_space s [] 0)

let of_input f =
  match ssplit (input_line f) with
  | [] -> Empty
  | "c" :: _ -> Comment
  | "p" :: "cnf" :: i :: j :: [] ->
    begin try
        Pcnf (int_of_string i, int_of_string j)
      with Failure _ ->
        raise (Syntax_error (-1))
    end
  | l ->
    begin try
        begin match List.rev_map int_of_string l with
          | 0 :: r -> Clause r
          | _ -> raise (Syntax_error (-1))
        end
      with Failure _ -> raise (Syntax_error (-1))
    end

let parse_with todo file =
  let f = open_in file in
  let line = ref 0 in
  try
    while true do
      incr line;
      todo (of_input f)
    done
  with
  | Syntax_error _ ->
    raise (Syntax_error !line)
  | End_of_file ->
    close_in f

let cnf = ref []
let parse_line = function
  | Empty | Comment | Pcnf _ -> ()
  | Clause l -> cnf := l :: !cnf

let parse f =
  cnf := [];
  parse_with parse_line f;
  !cnf
