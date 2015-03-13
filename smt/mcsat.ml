(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module Fsmt = Expr

module Tsmt = struct

  module M = Map.Make(Fsmt.Term)
  module CC = Cc.Make(String)

  (* Type definitions *)
  type term = Fsmt.Term.t
  type formula = Fsmt.t

  type proof = unit

  let proof_debug () =
      "Proof", [], ["..."], Some "PURPLE"

  type assumption =
    | Lit of formula
    | Assign of term * term

  type slice = {
    start : int;
    length : int;
    get : int -> assumption * int;
    push : formula list -> proof -> unit;
    propagate : formula -> int -> unit;
  }

  type level = {
      cc : CC.t;
      assign : (term * int) M.t;
  }

  type res =
    | Sat
    | Unsat of formula list * proof

  type eval_res =
    | Valued of bool * int
    | Unknown

  (* Functions *)
  let dummy = { cc = CC.empty; assign = M.empty; }

  let env = ref dummy

  let current_level () = !env

  let to_clause (a, b, l) =
      Log.debug 10 "Expl : %s; %s" a b;
      List.iter (fun s -> Log.debug 10 " |- %s" s) l;
      let rec aux acc = function
          | [] | [_] -> acc
          | x :: ((y :: _) as r) ->
                  aux (Fsmt.mk_eq x y :: acc) r
      in
      (Fsmt.mk_eq a b) :: (List.rev_map Fsmt.neg (aux [] l))

  let assume s =
      try
          for i = s.start to s.start + s.length - 1 do
              match s.get i with
              | (Assign (x, v)), lvl ->
                      env := { !env with assign = M.add x (v, lvl) !env.assign }
              | Lit f, _ ->
                Log.debug 10 "Propagating in th : %s" (Log.on_fmt Fsmt.print f);
                match f with
                | Fsmt.Prop _ -> ()
                | Fsmt.Equal (i, j) ->
                  env := { !env with cc = CC.add_eq !env.cc i j }
                | Fsmt.Distinct (i, j) ->
                  env := { !env with cc = CC.add_neq !env.cc i j }
          done;
          Sat
      with CC.Unsat x ->
        Log.debug 8 "Making explanation clause...";
        Unsat (to_clause x, ())

  let backtrack l = env := l

  let assign t = CC.repr !env.cc t

  let iter_assignable f = function
      | Fsmt.Prop _ -> ()
      | Fsmt.Equal(a, b)
      | Fsmt.Distinct (a, b) -> f a; f b

  let max (a: int) (b: int) = if a < b then b else a

  let eval = function
      | Fsmt.Prop _ -> Unknown
      | Fsmt.Equal (a, b) ->
              begin try
                  let a', lvl_a = M.find a !env.assign in
                  let b', lvl_b = M.find b !env.assign in
                  Valued (Fsmt.Term.equal a' b', max lvl_a lvl_b)
              with Not_found -> Unknown end
      | Fsmt.Distinct (a, b) ->
              begin try
                  let a', lvl_a = M.find a !env.assign in
                  let b', lvl_b = M.find b !env.assign in
                  Valued (not (Fsmt.Term.equal a' b'), max lvl_a lvl_b)
              with Not_found -> Unknown end

  let if_sat _ = ()

end

module Make(Dummy:sig end) = struct
  module SmtSolver = Mcsolver.Make(Log)(Fsmt)(Tsmt)

  type atom = Fsmt.t
  type clause = SmtSolver.St.clause
  type proof = SmtSolver.Proof.proof

  type res =
    | Sat
    | Unsat

  let solve () =
    try
      SmtSolver.solve ();
      Sat
    with SmtSolver.Unsat -> Unsat

  let assume l =
    try
      SmtSolver.assume l
    with SmtSolver.Unsat -> ()

  let get_proof () =
    (* SmtSolver.Proof.learn (SmtSolver.history ()); *)
    match SmtSolver.unsat_conflict () with
    | None -> assert false
    | Some c -> SmtSolver.Proof.prove_unsat c

  let eval = SmtSolver.eval

  let unsat_core = SmtSolver.Proof.unsat_core

  let print_atom = Fsmt.print
  let print_clause = SmtSolver.St.print_clause
  let print_proof = SmtSolver.Proof.print_dot
end
