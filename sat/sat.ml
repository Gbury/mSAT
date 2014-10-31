(* Copyright 2014 Guillaume Bury *)

module Fsat = struct
    exception Out_of_int

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

module Sat = struct
    module SatSolver = Solver.Make(Fsat)(Stypes)(Exp)(Tsat)

    type atom = Fsat.t
    type state = SatSolver.t

end
