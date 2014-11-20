(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

(* Misc directory/files manipulation *)
let list_dir s =
    let d = Unix.opendir s in
    let l = ref [] in
    try
        while true do
            l := (Filename.concat s (Unix.readdir d)) :: !l
        done;
        assert false
    with End_of_file ->
        !l

let list_dir_files_rec s =
    let rec aux acc = function
        | [] -> acc
        | f :: r ->
                let f' = Filename.basename f in
                if f' = Filename.current_dir_name || f' = Filename.parent_dir_name then
                    aux acc r
                else begin match Unix.((stat f).st_kind) with
                | Unix.S_REG -> aux (f :: acc) r
                | Unix.S_DIR -> aux acc (List.rev_append (list_dir f) r)
                | _ -> aux acc r
                end
    in
    aux [] [s]

(* Commit sha completion *)
let complete root sha =
    let l = list_dir root in
    let aux s = try String.sub s 0 (String.length sha) with Invalid_argument _ -> "" in
    let test s = sha = aux (Filename.basename s) in
    List.filter test l

let commit_info sha =
    let cmd = "git show "^ sha in
    let ch = Unix.open_process_in cmd in
    let l = (input_line ch :: input_line ch :: input_line ch :: input_line ch :: input_line ch :: input_line ch :: []) in
    ignore (Unix.close_process_in ch);
    String.concat "\n" (List.rev l)

let last_commit () =
    let ch = Unix.open_process_in "git rev-parse HEAD" in
    let s = input_line ch in
    ignore (Unix.close_process_in ch);
    s

(* Raw log file parsing *)
exception Empty_raw of string
exception Unknown_status of string * string list

type status =
    | Sat
    | Unsat
    | Timeout
    | Spaceout

type pb = {
    pb_name : string;
    pb_st : status;
    pb_time : float;
}

let str_cut s start len =
    try String.sub s start len
    with Invalid_argument _ -> ""

let status_of_lines f = function
    | ["Sat"] -> Sat
    | ["Unsat"] -> Unsat
    | ["Time limit exceeded"; _] -> Timeout
    | ["Size limit exceeded"; _] -> Spaceout
    | l ->
        Format.printf "For file '%s' : unknown return string :@\n" f;
        List.iter (fun s -> Format.printf "%s@." s) l;
        raise (Unknown_status (f, l))

let parse_raw base f =
    let f_in = open_in f in
    let f_lines = ref [] in
    begin try
        while true do
            f_lines := input_line f_in :: !f_lines
        done;
        assert false
    with End_of_file ->
        close_in f_in;
    end;
    match !f_lines with
    | [] -> raise (Empty_raw f)
    | s :: r ->
            let st = status_of_lines f (List.rev r) in
            assert (str_cut f 0 (String.length base) = base);
            let file_name = String.sub f (String.length base) (String.length f - String.length base) in
            { pb_name = file_name; pb_st = st; pb_time = float_of_string s }

let parse_commit root =
    let s = Filename.concat root "raw" in
    let l = list_dir_files_rec s in
    let res = Hashtbl.create (List.length l) in
    List.iter (fun f ->
        try
            let pb = parse_raw s f in
            Hashtbl.add res pb.pb_name pb
        with Empty_raw _ | Unknown_status _ -> ()
    ) l;
    res

