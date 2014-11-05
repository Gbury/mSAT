(* Copyright 2014 Guillaume Bury *)

module Fsat = struct
  exception Dummy
  exception Out_of_int

  (* Until the constant true_ and false_ are not needed anymore,
   * wa can't simply use sigend integers to represent literals *)
  type t = int

  let max_lit = min max_int (- min_int)
  let max_index = ref 0

  let make i =
    if i <> 0 then begin
      max_index := max !max_index (abs i);
      i
    end else
      raise Dummy

  let dummy = 0

  let neg a = - a
  let norm a = abs a, a < 0

  let hash (a:int) = Hashtbl.hash a
  let equal (a:int) b = a=b
  let compare (a:int) b = Pervasives.compare a b

  let _str = Hstring.make ""
  let label a = _str
  let add_label _ _ = ()

  let create, iter =
    let create () =
      if !max_index <> max_lit then
        (incr max_index; !max_index)
      else
        raise Out_of_int
    in
    let iter: (t -> unit) -> unit = fun f ->
      for j = 1 to !max_index do
        f j
      done
    in
    create, iter

  let print fmt a =
    Format.fprintf fmt "%s%d" (if a < 0 then "~" else "") (abs a)

end

module Stypes = Solver_types.Make(Fsat)

module Exp = Explanation.Make(Stypes)

module Tsat = struct
  (* We don't have anything to do since the SAT Solver already
   * does propagation and conflict detection *)

  type t = int
  type formula = Fsat.t
  type explanation = Exp.t
  type proof = unit

  exception Inconsistent of explanation

  let dummy = -1
  let empty () = 0
  let assume ~cs:_ _ _ _ = 0
end

module Make(L : Neperien.S) = struct
  module SatSolver = Solver.Make(Fsat)(Stypes)(Exp)(Tsat)(L)

  exception Bad_atom

  type res =
    | Sat
    | Unsat

  let _i = ref 0

  type atom = Fsat.t

  let new_atom () =
    try
      Fsat.create ()
    with Fsat.Out_of_int ->
      raise Bad_atom

  let make i =
    try
      Fsat.make i
    with Fsat.Dummy ->
      raise Bad_atom

  let neg = Fsat.neg

  let hash = Fsat.hash
  let equal = Fsat.equal
  let compare = Fsat.compare

  let print_atom = Fsat.print
  let iter_atoms = Fsat.iter

  let solve () =
    try
      SatSolver.solve ();
      Sat
    with SatSolver.Unsat _ -> Unsat

  let assume l =
    incr _i;
    try
      SatSolver.assume l !_i
    with SatSolver.Unsat _ -> ()

  let eval = SatSolver.eval
end
