(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

exception Incorrect_model
exception Out_of_time
exception Out_of_space

let file = ref ""
let p_cnf = ref false
let p_check = ref false
let p_dot_proof = ref ""
let p_proof_print = ref false
let time_limit = ref 300.
let size_limit = ref 1000_000_000.
let no_proof = ref false

module S = Msat_sat

module Process() = struct
  module D = Msat_backend.Dot.Make(S)(Msat_backend.Dot.Default(S))

  let hyps = ref []

  let st = S.create ~store_proof:(not !no_proof) ~size:`Big ()

  let check_model sat =
    let check_clause c =
      let l = List.map (function a ->
          Log.debugf 99
            (fun k -> k "Checking value of %a" S.Formula.pp a);
          sat.Msat.eval a) c in
      List.exists (fun x -> x) l
    in
    let l = List.map check_clause !hyps in
    List.for_all (fun x -> x) l

  let prove ~assumptions () =
    let res = S.solve ~assumptions st in
    let t = Sys.time () in
    begin match res with
      | S.Sat state ->
        if !p_check then
          if not (check_model state) then
            raise Incorrect_model;
        let t' = Sys.time () -. t in
        Format.printf "Sat (%f/%f)@." t t'
      | S.Unsat state ->
        if !p_check then (
          let p = state.Msat.get_proof () in
          S.Proof.check_empty_conclusion p;
          S.Proof.check p;
          if !p_dot_proof <> "" then (
            let oc = open_out !p_dot_proof in
            let fmt = Format.formatter_of_out_channel oc in
            Format.fprintf fmt "%a@?" D.pp p;
            flush oc; close_out_noerr oc;
          )
        );
        let t' = Sys.time () -. t in
        Format.printf "Unsat (%f/%f)@." t t'
    end

  let conv_c c = List.rev_map S.Int_lit.make c

  let add_clauses cs =
    S.assume st (CCList.map conv_c cs) ()
end[@@inline]

let parse_file f =
  let module L = Lexing in
  CCIO.with_in f
    (fun ic ->
       let buf =
         if CCString.suffix ~suf:".gz" f
         then (
           let gic = Gzip.open_in_chan ic in
           L.from_function (fun bytes len -> Gzip.input gic bytes 0 len)
         ) else L.from_channel ic
       in
       buf.L.lex_curr_p <- {buf.L.lex_curr_p with L.pos_fname=f;};
       Dimacs_parse.file Dimacs_lex.token buf)

let error_msg opt arg l =
  Format.fprintf Format.str_formatter "'%s' is not a valid argument for '%s', valid arguments are : %a"
    arg opt (fun fmt -> List.iter (fun (s, _) -> Format.fprintf fmt "%s, " s)) l;
  Format.flush_str_formatter ()

(* Arguments parsing *)
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
    with Failure _ -> raise (Arg.Bad "bad numeric argument")

let setup_gc_stat () =
  at_exit (fun () ->
      Gc.print_stat stdout;
    )

let input_file = fun s -> file := s

let usage = "Usage : main [options] <file>"
let argspec = Arg.align [
    "-bt", Arg.Unit (fun () -> Printexc.record_backtrace true),
    " Enable stack traces";
    "-cnf", Arg.Set p_cnf,
    " Prints the cnf used.";
    "-check", Arg.Set p_check,
    " Build, check and print the proof (if output is set), if unsat";
    "-dot", Arg.Set_string p_dot_proof,
    " If provided, print the dot proof in the given file";
    "-gc", Arg.Unit setup_gc_stat,
    " Outputs statistics about the GC";
    "-size", Arg.String (int_arg size_limit),
    "<s>[kMGT] Sets the size limit for the sat solver";
    "-time", Arg.String (int_arg time_limit),
    "<t>[smhd] Sets the time limit for the sat solver";
    "-v", Arg.Int (fun i -> Log.set_debug i),
    "<lvl> Sets the debug verbose level";
    "-no-proof", Arg.Set no_proof, " disable proof logging";
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

let main () =
  (* Administrative duties *)
  Arg.parse argspec input_file usage;
  if !file = "" then (
    Arg.usage argspec usage;
    exit 2
  );
  let al = Gc.create_alarm check in

  let module P = Process() in

  (* Interesting stuff happening *)
  let clauses = parse_file !file in
  P.add_clauses clauses;
  P.prove ~assumptions:[] ();
  Gc.delete_alarm al;
  ()

let () =
  try
    main ()
  with
  | Out_of_time ->
    Format.printf "Timeout@.";
    exit 2
  | Out_of_space ->
    Format.printf "Spaceout@.";
    exit 3
  | Incorrect_model ->
    Format.printf "Internal error : incorrect *sat* model@.";
    exit 4
  | S.Proof.Resolution_error msg  ->
    Format.printf "Internal error: incorrect *unsat* proof:\n%s@." msg;
    exit 5

