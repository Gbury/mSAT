
module S = Sat.Make(struct end)

exception Out_of_time
exception Out_of_space

(* Arguments parsing *)
let file = ref ""
let p_assign = ref false
let time_limit = ref 300.
let size_limit = ref 1000_000_000.
;;

let int_arg r arg =
  let l = String.length arg in
  let multiplier m =
    let arg1 = String.sub arg 0 (l-1) in
    r := m *. (float_of_string arg1)
  in
  if l = 0 then raise (Arg.Bad "bad numeric argument")
  else
    try
      match arg.[l-1] with
      | 'k' -> multiplier 1e3
      | 'M' -> multiplier 1e6
      | 'G' -> multiplier 1e9
      | 'T' -> multiplier 1e12
      | 's' -> multiplier 1.
      | 'm' -> multiplier 60.
      | 'h' -> multiplier 3600.
      | 'd' -> multiplier 86400.
      | '0'..'9' -> r := float_of_string arg
      | _ -> raise (Arg.Bad "bad numeric argument")
    with Failure "float_of_string" -> raise (Arg.Bad "bad numeric argument")
;;

let input_file = fun s -> file := s
let usage = "Usage : main [options] <file>"
let argspec = Arg.align [
    "-v", Arg.Int (fun i -> Log.set_debug i),
    "<lvl> Sets the debug verbose level";
    "-t", Arg.String (int_arg time_limit),
    "<t>[smhd] Sets the time limit for the sat solver";
    "-s", Arg.String (int_arg size_limit),
    "<s>[kMGT] Sets the size limit for the sat solver";
    "-model", Arg.Set p_assign,
    " Outputs the boolean model found if sat";
  ]

(* Limits alarm *)
let check () =
  let t = Sys.time () in
  let heap_size = (Gc.quick_stat ()).Gc.heap_words in
  let s = float heap_size *. float Sys.word_size /. 8. in
  if t > !time_limit then
    raise Out_of_time
  else if s > !size_limit then
    raise Out_of_space

(* Entry file parsing *)
let get_cnf () =
  List.map (List.map S.make) (Parser.parse !file)

let print_cnf cnf =
  Format.printf "CNF :@\n";
  List.iter (fun c ->
      Format.fprintf Format.std_formatter "%a;@\n"
        (fun fmt -> List.iter (fun a ->
             Format.fprintf fmt "%a@ " S.print_atom a
           )
        ) c
    ) cnf

(* Iterate over all variables created *)
let print_assign fmt () =
  S.iter_atoms (fun a ->
      Format.fprintf fmt "%a -> %s,@ "
        S.print_atom a
        (if S.eval a then "T" else "F")
    )

let main () =
  (* Administrative duties *)
  Arg.parse argspec input_file usage;
  if !file = "" then begin
    Arg.usage argspec usage;
    exit 2
  end;
  let al = Gc.create_alarm check in

  (* Interesting stuff happening *)
  let cnf = get_cnf () in
  S.assume cnf;
  match S.solve () with
  | S.Sat ->
    Format.printf "Sat@.";
    if !p_assign then
      print_assign Format.std_formatter ()
  | S.Unsat ->
    Format.printf "Unsat@."
;;

try
  main ()
with
| Out_of_time ->
  Format.printf "Time limit exceeded@.";
  exit 2
| Out_of_space ->
  Format.printf "Size limit exceeded@.";
  exit 3
