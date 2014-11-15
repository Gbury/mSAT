(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module Fsmt = struct

  exception Invalid_var

  type var = string
  type t =
    | Prop of int
    | Equal of var * var
    | Distinct of var * var

  let dummy = Prop 0

  let max_fresh = ref 0
  let fresh () =
    incr max_fresh;
    Prop (2 * !max_fresh + 1)

  let mk_prop i =
      if i <> 0 && i < max_int / 2 then Prop (2 * i)
      else raise Invalid_var

  let mk_var i =
    if i <> "" then i
    else raise Invalid_var

  let mk_eq i j = Equal (mk_var (min i j), mk_var (max i j))
  let mk_neq i j = Distinct (mk_var (min i j), mk_var (max i j))

  let neg = function
    | Prop i -> Prop (-i)
    | Equal (a, b) -> Distinct (a, b)
    | Distinct (a, b) -> Equal (a, b)

  let norm = function
    | Prop i -> Prop (abs i), i < 0
    | Equal (a, b) -> Equal (a, b), false
    | Distinct (a, b) -> Equal (a, b), true

  (* Only used after normalisation, so usual functions should work *)
  let hash = Hashtbl.hash
  let equal = (=)
  let compare = Pervasives.compare

  let s = Hstring.make ""
  let label _ = s
  let add_label _ _ = ()

  let print fmt = function
    | Prop i -> Format.fprintf fmt "%s%s%d" (if i < 0 then "¬ " else "") (if i mod 2 = 0 then "v" else "f") (abs i)
    | Equal (a, b) -> Format.fprintf fmt "%s = %s" a b
    | Distinct (a, b) -> Format.fprintf fmt "%s ≠ %s" a b

end

module Tseitin = Tseitin.Make(Fsmt)

module Tsmt = struct

  module CC = Cc.Make(String)

  type formula = Fsmt.t
  type proof = unit
  type slice = {
    start : int;
    length : int;
    get : int -> formula;
    push : formula -> formula list -> proof -> unit;
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
              | Fsmt.Equal (i, j) as f ->
                      env := CC.add_eq !env i j
              | Fsmt.Distinct (i, j) as f ->
                      env := CC.add_neq !env i j
              | _ -> assert false
          done;
          Sat (current_level ())
      with CC.Unsat x ->
          Log.debug 8 "Making explanation clause...";
          Unsat (to_clause x, ())

  let backtrack l = env := l

end

module Make(Dummy:sig end) = struct
  module SmtSolver = Solver.Make(Fsmt)(Tsmt)

  type atom = Fsmt.t
  type clause = SmtSolver.St.clause
  type proof = SmtSolver.Proof.proof

  type res =
    | Sat
    | Unsat

  let _i = ref 0
  let solve () =
    try
      SmtSolver.solve ();
      Sat
    with SmtSolver.Unsat -> Unsat

  let assume l =
    incr _i;
    try
      SmtSolver.assume l !_i
    with SmtSolver.Unsat -> ()

  let get_proof () =
    SmtSolver.Proof.learn (SmtSolver.history ());
    match SmtSolver.unsat_conflict () with
    | None -> assert false
    | Some c -> SmtSolver.Proof.prove_unsat c

  let unsat_core = SmtSolver.Proof.unsat_core

  let print_atom = Fsmt.print
  let print_clause = SmtSolver.St.print_clause
  let print_proof = SmtSolver.Proof.print_dot
end
