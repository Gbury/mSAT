(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module Fsat = struct
  exception Dummy of int

  type t = int
  type proof = unit

  let max_lit = max_int
  let max_fresh = ref (-1)
  let max_index = ref 0

  let _make i =
    if i <> 0 && (abs i) < max_lit then begin
      max_index := max !max_index (abs i);
      i
    end else
      (Format.printf "Warning : %d/%d@." i max_lit;
       raise (Dummy i))

  let dummy = 0

  let neg a = - a
  let norm a = abs a, a < 0
  let abs = abs
  let sign i = i > 0
  let apply_sign b i = if b then i else neg i
  let set_sign b i = if b then abs i else neg (abs i)

  let hash (a:int) = a land max_int
  let equal (a:int) b = a=b
  let compare (a:int) b = Pervasives.compare a b

  let _str = Hstring.make ""
  let label a = _str
  let add_label _ _ = ()

  let make i = _make (2 * i)
  let fresh, iter =
    let create () =
      incr max_fresh;
      _make (2 * !max_fresh + 1)
    in
    let iter: (t -> unit) -> unit = fun f ->
      for j = 1 to !max_index do
        f j
      done
    in
    create, iter

  let print fmt a =
    Format.fprintf fmt "%s%s%d"
      (if a < 0 then "~" else "")
      (if a mod 2 = 0 then "v" else "f")
      ((abs a) / 2)
end

module Tseitin = Tseitin.Make(Fsat)

module Make(Dummy : sig end) = struct
  module Tsat = Solver.DummyTheory(Fsat)
  module SatSolver = Solver.Make(Fsat)(Tsat)

  exception Bad_atom

  type atom = Fsat.t
  type clause = SatSolver.St.clause
  type proof = SatSolver.Proof.proof

  let tag_clause cl = SatSolver.St.(cl.tag)

  type res =
    | Sat
    | Unsat

  let new_atom () =
    try
      Fsat.fresh ()
    with Fsat.Dummy _ ->
      raise Bad_atom

  let make i =
    try
      Fsat.make i
    with Fsat.Dummy _ ->
      raise Bad_atom


  let abs = Fsat.abs
  let neg = Fsat.neg
  let sign = Fsat.sign
  let apply_sign = Fsat.apply_sign
  let set_sign = Fsat.set_sign

  let hash = Fsat.hash
  let equal = Fsat.equal
  let compare = Fsat.compare

  let iter_atoms = Fsat.iter

  let solve () =
    try
      SatSolver.solve ();
      Sat
    with SatSolver.Unsat -> Unsat

  let assume ?tag l =
    try
      SatSolver.assume ?tag l
    with SatSolver.Unsat -> ()

  let eval = SatSolver.eval
  let eval_level = SatSolver.eval_level

  let get_proof () =
    (* SatSolver.Proof.learn (SatSolver.history ()); *)
    match SatSolver.unsat_conflict () with
    | None -> assert false
    | Some c -> SatSolver.Proof.prove_unsat c

  let unsat_core = SatSolver.Proof.unsat_core

  let print_atom = Fsat.print
  let print_clause = SatSolver.St.print_clause
  let print_proof out p =
    let module Dot = Dot.Make(SatSolver.Proof)(struct
        let clause_name c = SatSolver.St.(c.name)
        let print_atom = SatSolver.St.print_atom
        let lemma_info () = "()", None, []
      end)
    in
    Dot.print out p
end
