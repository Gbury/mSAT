
exception Out_of_time
exception Out_of_space

(* Arguments parsing *)
let file = ref ""
let p_assign = ref false
let time_limit = ref 300.
let size_limit = ref 1000_000_000.

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

let setup_gc_stat () =
  at_exit (fun () ->
    Gc.print_stat stdout;
  )

let input_file = fun s -> file := s
let log_file = ref ""
let debug_level = ref max_int
let usage = "Usage : main [options] <file>"
let argspec = Arg.align [
    "-v", Arg.Set_int debug_level,
      "<lvl> Sets the debug verbose level";
    "-log", Arg.Set_string log_file,
      "<file> Sets the log file";
    "-t", Arg.String (int_arg time_limit),
      "<t>[smhd] Sets the time limit for the sat solver";
    "-s", Arg.String (int_arg size_limit),
      "<s>[kMGT] Sets the size limit for the sat solver";
    "-model", Arg.Set p_assign,
      " Outputs the boolean model found if sat";
    "-gc", Arg.Unit setup_gc_stat,
      " Outputs statistics about the GC";
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

let proceed () =
  (* logging module *)
  let log = match !log_file with
    | "" -> Neperien.dummy
    | filename -> Neperien.log_to_file_mod_exn filename
  in
  let module NLog = (val log : Neperien.S) in
  NLog.set_max_level !debug_level;
  let module S = Sat.Make(NLog) in

  (* Entry file parsing *)
  let get_cnf () =
    List.rev_map (List.rev_map S.make) (Parser.parse !file)
  in

  let print_cnf cnf =
    Format.printf "CNF :@\n";
    List.iter (fun c ->
        Format.fprintf Format.std_formatter "%a;@\n"
          (fun fmt -> List.iter (fun a ->
               Format.fprintf fmt "%a@ " S.print_atom a
             )
          ) c
      ) cnf
  in

  (* Iterate over all variables created *)
  let print_assign fmt () =
    S.iter_atoms (fun a ->
        Format.fprintf fmt "%a -> %s,@ "
          S.print_atom a
          (if S.eval a then "T" else "F")
      )
  in

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

let main () =
  (* Administrative duties *)
  Arg.parse argspec input_file usage;
  if !file = "" then begin
    Arg.usage argspec usage;
    exit 2
  end;
  ignore(Gc.create_alarm check);
  proceed ()

let () =
  try
    main ()
  with
  | Out_of_time ->
    Format.printf "Time limit exceeded@.";
    exit 2
  | Out_of_space ->
    Format.printf "Size limit exceeded@.";
    exit 3
