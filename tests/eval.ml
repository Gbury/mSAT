(** Random tests *)

open Aez

let hs = Hstring.make

module S = Smt.Make(struct end)

let ty = Smt.Type.type_proc;; (hs "tau");;
Smt.Symbol.declare (hs "a") [] ty;;
Smt.Symbol.declare (hs "b") [] ty;;
Smt.Symbol.declare (hs "c") [] ty;;

let a = Smt.Term.make_app (hs "a") [];;
let b = Smt.Term.make_app (hs "b") [];;
let c = Smt.Term.make_app (hs "c") [];;

S.assume ~id:0 Smt.Formula.(make_imply (make_pred ~sign:false a) (make_pred b));;
S.assume ~id:1 Smt.Formula.(make_imply (make_pred b) (make_pred c));;
S.assume ~id:2 Smt.Formula.(make_not (make_pred c));;

try
  S.check ();
  Printf.printf "problem is sat, checking model...\n";
  let v_a = S.eval a in
  let v_b = S.eval b in
  let v_c = S.eval c in
  Printf.printf "a=%B, b=%B, c=%B\n" v_a v_b v_c;
  assert (v_a && not v_b && not v_c);
  ()
with Smt.Unsat l ->
  Printf.printf "problem deemed unsat, shouldn't be\n"
