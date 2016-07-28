(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

(* Tests that require the API *)

module F = Expr
module T = Cnf.S

let (|>) x f = f x

type solver = Smt | Mcsat
let solver_list = [
  "smt", Smt;
  "mcsat", Mcsat;
]

let solver = ref Smt
let p_check = ref false
let time_limit = ref 300.
let size_limit = ref 1000_000_000.

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

let usage = "Usage : test_api [options]"
let argspec = Arg.align [
    "-check", Arg.Set p_check,
    " Build, check and print the proof (if output is set), if unsat";
    "-s", Arg.String set_solver,
    "{smt,mcsat} Sets the solver to use (default smt)";
    "-v", Arg.Int (fun i -> Log.set_debug i),
    "<lvl> Sets the debug verbose level";
  ]

let string_of_solver = function
  | Mcsat -> "mcsat"
  | Smt -> "smt"

type solver_res =
  | R_sat
  | R_unsat

module type BASIC_SOLVER = sig
  val solve : ?assumptions:F.t list -> unit -> solver_res
  val assume : ?tag:int -> F.t list list -> unit
end

exception Incorrect_model

let mk_solver (s:solver): (module BASIC_SOLVER) =
  match s with
    | Smt ->
      let module S = struct
        include Smt.Make(struct end)
        let solve ?assumptions ()= match solve ?assumptions() with
          | Sat _ ->
            R_sat
          | Unsat us ->
            let p = us.Solver_intf.get_proof () in
            Proof.check p;
            R_unsat
      end
      in (module S)
    | Mcsat ->
      let module S = struct
        include Mcsat.Make(struct end)
        let solve ?assumptions ()= match solve ?assumptions() with
          | Sat _ ->
            R_sat
          | Unsat us ->
            let p = us.Solver_intf.get_proof () in
            Proof.check p;
            R_unsat
      end
      in (module S)

exception Error of string

let error msg = raise (Error msg)
let errorf msg = Format.ksprintf error msg

module Test = struct
  type action =
    | A_assume of F.t list list
    | A_solve of F.t list * [`Expect_sat | `Expect_unsat]

  type t = {
    name: string;
    actions: action list;
  }

  let mk_test name l = {name; actions=l}
  let assume l = A_assume (List.map (List.map F.mk_prop) l)
  let assume1 c = assume [c]
  let solve ?(assumptions=[]) e =
    let assumptions = List.map F.mk_prop assumptions in
    A_solve (assumptions, e)

  type result =
    | Pass
    | Fail of string

  let run (solver:solver) (t:t): result =
  (* Interesting stuff happening *)
    let (module S: BASIC_SOLVER) = mk_solver solver in
    try
      List.iter
        (function
          | A_assume cs ->
            S.assume cs
          | A_solve (assumptions, expect) ->
            match S.solve ~assumptions (), expect with
              | R_sat, `Expect_sat
              | R_unsat, `Expect_unsat -> ()
              | R_unsat, `Expect_sat ->
                error "expect sat, got unsat"
              | R_sat, `Expect_unsat ->
                error "expect unsat, got sat"
        )
        t.actions;
      Pass
    with Error msg ->
      Fail msg

  (* basic test *)
  let test1 =
    [ assume [[-1;2]; [-1;3]];
      solve `Expect_sat;
      assume [[-2;4]; [-3;-4]];
      solve `Expect_sat;
      assume [[1]];
      solve `Expect_unsat;
    ] |> mk_test "test1"

  (* same as test1 but with assumptions *)
  let test2 =
    [ solve `Expect_sat;
      assume [[-1;2]; [-1;3]];
      solve `Expect_sat;
      assume [[-2;4]; [-3;-4]];
      solve `Expect_sat;
      solve ~assumptions:[1] `Expect_unsat;
      solve `Expect_sat;
    ] |> mk_test "test2"

  (* repeat assumptions *)
  let test3 =
    [ assume [[-1;2]; [-1;3]];
      solve `Expect_sat;
      assume [[-2;4]; [-3;-4]];
      solve `Expect_sat;
      solve ~assumptions:[1] `Expect_unsat;
      solve `Expect_sat;
      solve ~assumptions:[1] `Expect_unsat;
      solve `Expect_sat;
      solve ~assumptions:[1] `Expect_unsat;
      solve `Expect_sat;
      solve ~assumptions:[2] `Expect_sat;
      assume [[1]];
      solve `Expect_unsat;
    ] |> mk_test "test3"

  (* just check that we do create new solvers *)
  let test_clean =
    [ solve `Expect_sat
    ] |> mk_test "test_clean"

  let suite =
    [ test1;
      test2;
      test3;
      test_clean; (* after test3 *)
    ]
end

let main () =
  (* Administrative duties *)
  Arg.parse argspec (fun _ -> ()) usage;
  let failed = ref false in
  List.iter
    (fun solver ->
       List.iter
         (fun test ->
            Printf.printf "(%-6s) %-10s... %!" (string_of_solver solver) test.Test.name;
            match Test.run solver test with
              | Test.Pass -> Printf.printf "ok\n%!"
              | Test.Fail msg ->
                failed := true;
                Printf.printf "fail (%s)\n%!" msg)
         Test.suite)
    [Smt; Mcsat];
  if !failed then exit 1

let () = main()
