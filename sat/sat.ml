(* Copyright 2014 Guillaume Bury *)

module Fsat = struct
    exception Out_of_int

    (* Until the constant true_ and false_ are not needed anymore,
     * wa can't simply use sigend integers to represent literals *)
    type t = {
        (* Invariant : var >= 0 *)
        var : int;
        pos : bool;
    }

    let max_index = ref 0

    let true_ = { var = 0; pos = true }
    let false_ = { var = 0; pos = false }
    let dummy = { var = -1; pos = true }

    let unsafe_make i = { var = i; pos = true }
    let make i = if i > 0 then unsafe_make i else dummy

    let neg a = { a with pos = not a.pos }
    let norm a = a, false

    let hash = Hashtbl.hash
    let equal = (=)
    let compare = Pervasives.compare

    let label a = Hstring.make ""
    let add_label _ _ = ()

    let create, iter =
        let create () =
            if !max_index <> max_int then
                (incr max_index; unsafe_make !max_index)
            else
                raise Out_of_int
        in
        let iter: (t -> unit) -> unit = fun f ->
            for j = 1 to !max_index do
                f (unsafe_make j)
            done
        in
        create, iter

    let print fmt a =
        Format.fprintf fmt "%s%d" (if not a.pos then "~" else "") a.var

end

module Stypes = Solver_types.Make(Fsat)

module Exp = Explanation.Make(Stypes)

module Tsat = struct
    (* We don't have anything to do since the SAT Solver already
     * does propagation and conflict detection *)

    type t = unit
    type formula = Fsat.t
    type explanation = Exp.t

    exception Inconsistent of explanation

    let empty () = ()
    let assume ~cs:_ _ _ _ = ()
end

module Make(Dummy : sig end) = struct
    module SatSolver = Solver.Make(Fsat)(Stypes)(Exp)(Tsat)

    type res =
        | Sat
        | Unsat

    let _i = ref 0

    type atom = Fsat.t
    type state = SatSolver.t

    let neg = Fsat.neg
    let new_atom = Fsat.create

    let hash = Fsat.hash
    let equal = Fsat.equal
    let compare = Fsat.compare

    let print_atom = Fsat.print
    let iter_atoms = Fsat.iter

    let solve () =
        try
            SatSolver.solve ();
            assert false
        with
        | SatSolver.Sat -> Sat
        | SatSolver.Unsat _ -> Unsat

    let assume l =
        incr _i;
        SatSolver.assume l !_i

    let eval = SatSolver.eval
end
