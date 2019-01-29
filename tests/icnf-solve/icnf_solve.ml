
module Vec = Msat.Vec

module Parse : sig
  type 'a event =
    | Add_clause of 'a array
    | Solve of 'a array

  type 'a t

  val make : file:string -> (int -> 'a) -> 'a t

  val next : 'a t -> 'a event (** @raise End_of_file when done *)
end = struct
  module L = Lexer

  type 'a event =
    | Add_clause of 'a array
    | Solve of 'a array

  type 'a t = {
    mk: int -> 'a;
    vec: 'a Vec.t;
    lex: Lexing.lexbuf;
  }

  let make ~file mk : _ t =
    let ic = open_in file in
    let lex = Lexing.from_channel ic in
    at_exit (fun () -> close_in_noerr ic);
    {lex; vec=Vec.create(); mk; }

  let rec next (self:_ t) : _ event =
    match L.token self.lex with
    | L.EOF -> raise End_of_file
    | L.A ->
      let c = read_ints self in
      Solve c
    | L.I 0 ->
      Add_clause [| |]
    | L.I x ->
      let c = read_ints ~first:(self.mk x) self in
      Add_clause c
  and read_ints ?first self : _ array =
    Vec.clear self.vec; (* reuse local vec *)
    CCOpt.iter (Vec.push self.vec) first;
    let rec aux() =
      match L.token self.lex with
      | L.I 0 -> Vec.to_array self.vec (* done *)
      | L.I n ->
        let x = self.mk n in
        Vec.push self.vec x;
        aux()
      | L.A -> failwith "unexpected \"a\""
      | L.EOF -> failwith "unexpected end of file"
    in
    aux()
end

module Solver = struct
  module F = Msat_sat.Int_lit
  module S = Msat_sat
  type t = S.t

  let make () = S.create()
  let mklit s i = S.make_atom s (let v = F.make (abs i) in if i>0 then v else F.neg v)
  let add_clause s c = S.add_clause_a s c (); true
  let to_int a : int = F.to_int @@ S.Atom.formula a
  let solve s ass =
    let ass = Array.to_list ass in
    match S.solve ~assumptions:ass s with
    | S.Sat _ -> Ok ()
    | S.Unsat { unsat_assumptions; _ } ->
      let core = unsat_assumptions() in
      Error core
end

let solve_with_solver ~check ~debug file : unit =
  Printf.eprintf "c process %S\n%!" file;
  let s = Solver.make () in
  let pp_arr out a =
    Array.iter (fun lit -> Printf.fprintf out "%d " (Solver.to_int lit)) a;
  in
  let p = Parse.make ~file (Solver.mklit s) in
  let rec process_problem () =
    match Parse.next p with
    | Parse.Add_clause c ->
      if debug then (
        Printf.printf "add_clause %a\n%!" pp_arr c;
        Msat.Log.set_debug 5;
      );
      let r = Solver.add_clause s c in
      if r then process_problem()
      else (
        Printf.printf "UNSAT\n%!";
        skip_problem ()
      )
    | Parse.Solve assumptions ->
      if debug then (
        Printf.printf "c solve %a\n%!" pp_arr assumptions;
      );
      begin match Solver.solve s assumptions with
        | Ok () -> Printf.printf "SAT\n%!"
        | Error (_::_ as core) when check ->
          Printf.printf "UNSAT\n%!";
          let core = Array.of_list core in
          if debug then Printf.printf "check unsat core %a\n" pp_arr core;
          (* check unsat core *)
          begin match Solver.solve s core with
            | Ok () ->
              Printf.printf "error: unsat core %a is SAT\n%!" pp_arr core;
              exit 1
            | Error _ -> ()
          end;
        | Error _ ->
          Printf.printf "UNSAT\n%!";
      end;
      (* next problem! *)
      process_problem()
    | exception End_of_file ->
      done_ ()
  and skip_problem() =
    match Parse.next p with
    | Parse.Add_clause _ -> skip_problem()
    | Parse.Solve _ -> process_problem ()
    | exception End_of_file -> done_ ()
  and done_ () =
    Printf.eprintf "c done for %S\n%!" file;
    ()
  in
  process_problem ()

let solve_with_file ~check ~debug file : unit =
  try solve_with_solver ~check ~debug file
  with e ->
    Printf.printf "error while solving %S:\n%s"
    file (Printexc.to_string e);
    exit 1

let () =
  let files = ref [] in
  let debug = ref false in
  let check = ref false in
  let opts = [
    "-d", Arg.Set debug, " debug";
    "--check", Arg.Set check, " check unsat cores";
  ] |> Arg.align in
  Arg.parse opts (fun f -> files := f :: !files) "icnf_solve [options] <file>";
  List.iter (fun f -> solve_with_file ~check:!check ~debug:!debug f) !files;
  ()
