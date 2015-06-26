(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module Fsmt = Expr

module Tsmt = struct

  module CC = Cc.Make(String)

  type formula = Fsmt.t
  type proof = unit
  type slice = {
    start : int;
    length : int;
    get : int -> formula;
    push : formula list -> proof -> unit;
  }
  type level = CC.t

  type res =
    | Sat of level
    | Unsat of formula list * proof

  let dummy = CC.empty

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
              Log.debug 10 "Propagating in th : %s" (Log.on_fmt Fsmt.print (s.get i));
              match s.get i with
              | Fsmt.Prop _ -> ()
              | Fsmt.Equal (i, j) -> env := CC.add_eq !env i j
              | Fsmt.Distinct (i, j) -> env := CC.add_neq !env i j
          done;
          Sat (current_level ())
      with CC.Unsat x ->
          Log.debug 8 "Making explanation clause...";
          Unsat (to_clause x, ())

  let backtrack l = env := l

end

module Make(Dummy:sig end) = struct
  module SmtSolver = Solver.Make(Log)(Fsmt)(Tsmt)

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
    match SmtSolver.unsat_conflict () with
    | None -> assert false
    | Some c -> SmtSolver.Proof.prove_unsat c

  let eval = SmtSolver.eval

  let unsat_core = SmtSolver.Proof.unsat_core

  let print_atom = Fsmt.print
  let print_clause = SmtSolver.St.print_clause
  let print_proof = SmtSolver.Proof.print_dot
end
