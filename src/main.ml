(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module Sat = Sat.Make(struct end)
module Smt = Smt.Make(struct end)
module Mcsat = Mcsat.Make(struct end)

module P =
  Dolmen.Logic.Make(Dolmen.ParseLocation)
    (Dolmen.Id)(Dolmen.Term)(Dolmen.Statement)

exception Incorrect_model
exception Out_of_time
exception Out_of_space

type solver =
  | Sat
  | Smt
  | Mcsat

let solver = ref Smt
let solver_list = [
  "sat", Sat;
  "smt", Smt;
  "mcsat", Mcsat;
]

let error_msg opt arg l =
  Format.fprintf Format.str_formatter "'%s' is not a valid argument for '%s', valid arguments are : %a"
    arg opt (fun fmt -> List.iter (fun (s, _) -> Format.fprintf fmt "%s, " s)) l;
  Format.flush_str_formatter ()

let set_flag opt arg flag l =
  try
    flag := List.assoc arg l
  with Not_found ->
    invalid_arg (error_msg opt arg l)

let set_solver s = set_flag "Solver" s solver solver_list

(* Arguments parsing *)
let file = ref ""
let p_cnf = ref false
let p_check = ref false
let p_proof_print = ref false
let p_unsat_core = ref false
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
    "-gc", Arg.Unit setup_gc_stat,
    " Outputs statistics about the GC";
    "-s", Arg.String set_solver,
    "{sat,smt,mcsat} Sets the solver to use (default smt)";
    "-size", Arg.String (int_arg size_limit),
    "<s>[kMGT] Sets the size limit for the sat solver";
    "-time", Arg.String (int_arg time_limit),
    "<t>[smhd] Sets the time limit for the sat solver";
    "-u", Arg.Set p_unsat_core,
    " Prints the unsat-core explanation of the unsat proof (if used with -check)";
    "-v", Arg.Int (fun i -> Log.set_debug i),
    "<lvl> Sets the debug verbose level";
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

module Make
    (T : Type.S)
    (S : External.S with type St.formula = T.atom) = struct

  let do_task s =
    match s.Dolmen.Statement.descr with
    | Dolmen.Statement.Def (id, t) -> T.def id t
    | Dolmen.Statement.Decl (id, t) -> T.decl id t
    | Dolmen.Statement.Consequent t ->
      let cnf = T.consequent t in
      S.assume cnf
    | Dolmen.Statement.Antecedent t ->
      let cnf = T.antecedent t in
      S.assume cnf
    | Dolmen.Statement.Prove ->
      begin match S.solve () with
        | S.Sat _ -> ()
        | S.Unsat _ -> ()
      end
    | _ ->
      Format.printf "Command not supported:@\n%a@."
        Dolmen.Statement.print s
end


let main () =
  (* Administrative duties *)
  Arg.parse argspec input_file usage;
  if !file = "" then begin
    Arg.usage argspec usage;
    exit 2
  end;
  let al = Gc.create_alarm check in

  (* Interesting stuff happening *)
  let _, _input = P.parse_file !file in
  Gc.delete_alarm al;
  ()

(* Old code ...

   let cnf = get_cnf () in
   if !p_cnf then
   print_cnf cnf;
   match !solver with
   | Smt ->
   Smt.assume cnf;
   let res = Smt.solve () in
   Gc.delete_alarm al;
   begin match res with
    | Smt.Sat sat ->
      let t = Sys.time () in
      if !p_check then
        if not (List.for_all (List.exists sat.Solver_intf.eval) cnf) then
          raise Incorrect_model;
      print "Sat (%f/%f)" t (Sys.time () -. t)
    | Smt.Unsat us ->
      let t = Sys.time () in
      if !p_check then begin
        let p = us.Solver_intf.get_proof () in
        Smt.Proof.check p;
        print_proof p;
        if !p_unsat_core then
          print_unsat_core (Smt.unsat_core p)
      end;
      print "Unsat (%f/%f)" t (Sys.time () -. t)
   end
   | Mcsat ->
   Mcsat.assume cnf;
   let res = Mcsat.solve () in
   begin match res with
    | Mcsat.Sat sat ->
      let t = Sys.time () in
      if !p_check then
        if not (List.for_all (List.exists sat.Solver_intf.eval) cnf) then
          raise Incorrect_model;
      print "Sat (%f/%f)" t (Sys.time () -. t)
    | Mcsat.Unsat us ->
      let t = Sys.time () in
      if !p_check then begin
        let p = us.Solver_intf.get_proof () in
        Mcsat.Proof.check p;
        print_mcproof p;
        if !p_unsat_core then
          print_mc_unsat_core (Mcsat.unsat_core p)
      end;
      print "Unsat (%f/%f)" t (Sys.time () -. t)
   end

*)

let () =
  try
    main ()
  with
  | Incorrect_model ->
    Format.printf "Internal error : incorrect *sat* model@.";
    exit 4
  | Out_of_time ->
    Format.printf "Timeout@.";
    exit 2
  | Out_of_space ->
    Format.printf "Spaceout@.";
    exit 3

