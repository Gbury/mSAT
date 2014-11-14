(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module Fsmt = struct

  exception Invalid_var

  type var = string
  type t =
    | Fresh of int
    | Equal of var * var
    | Distinct of var * var

  let dummy = Fresh 0

  let max_fresh = ref 0
  let fresh () =
    incr max_fresh;
    Fresh !max_fresh

  let mk_var i =
    if i <> "" then
      i
    else
      raise Invalid_var

  let mk_eq i j = Equal (mk_var i, mk_var j)
  let mk_neq i j = Distinct (mk_var i, mk_var j)

  let neg = function
    | Fresh i -> Fresh (-i)
    | Equal (a, b) -> Distinct (a, b)
    | Distinct (a, b) -> Equal (a, b)

  let norm = function
    | Fresh i -> Fresh (abs i), i < 0
    | Equal (a, b) -> Equal (min a b, max a b), false
    | Distinct (a, b) -> Equal (min a b, max a b), true

  (* Only used after normalisation, so usual functions should work *)
  let hash = Hashtbl.hash
  let equal = (=)
  let compare = Pervasives.compare

  let s = Hstring.make ""
  let label _ = s
  let add_label _ _ = ()

  let print fmt = function
    | Fresh i -> Format.fprintf fmt "%sv%d" (if i < 0 then "¬ " else "") (abs i)
    | Equal (a, b) -> Format.fprintf fmt "%s = %s" a b
    | Distinct (a, b) -> Format.fprintf fmt "%s <> %s" a b

end

module Tseitin = Tseitin.Make(Fsmt)

module Tsmt = struct

  module U = Unionfind.Make(String)

  type formula = Fsmt.t
  type proof = unit
  type slice = {
    start : int;
    length : int;
    get : int -> formula;
    push : formula -> formula list -> proof -> unit;
  }
  type level = U.t

  type res =
    | Sat of level
    | Unsat of formula list * proof

  let dummy = U.empty

  let env = ref U.empty

  let current_level () = !env

  let temp = ref []

  let assume s =
      try
          for i = s.start to s.start + s.length - 1 do
              match s.get i with
              | Fsmt.Fresh _ -> ()
              | Fsmt.Equal (i, j) as f ->
                      temp := f :: !temp;
                      env := U.union !env i j
              | Fsmt.Distinct (i, j) as f ->
                      temp := f :: !temp;
                      env := U.forbid !env i j
              | _ -> assert false
          done;
          Sat (current_level ())
      with U.Unsat ->
          match !temp with
          | _ :: _ -> Unsat (List.rev_map Fsmt.neg !temp, ())
          | _ -> assert false

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

  let make_eq = Fsmt.mk_eq
  let make_neq = Fsmt.mk_neq

  let neg = Fsmt.neg

  let hash = Fsmt.hash
  let equal = Fsmt.equal
  let compare = Fsmt.compare

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
