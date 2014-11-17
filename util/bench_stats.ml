(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

exception Commit_not_found
exception Commit_ambiguous

(* Arguments parsing *)
let usage = "Usage : ./bench_stats [options] [commit1 [commit2]]"

let arg_commit = ref []
let anon s = arg_commit := s :: !arg_commit

let info_commit = ref false
let long_diff = ref false
let args = Arg.align [
    "-info", Arg.Set info_commit,
    " Adds info on the commit when printing stats";
    "-long", Arg.Set long_diff,
    " Print a long diff instead of a short one";
]

(* Access functions *)
let get_commit s =
    let d = Filename.concat (Filename.dirname Sys.argv.(0)) "bench" in
    match Parselog.complete (Filename.concat d "logs") s with
    | [c] -> Filename.basename c, Parselog.parse_commit c
    | [] -> raise Commit_not_found
    | _ -> raise Commit_ambiguous


(* Stats computing *)
type commit_stats = {
    nb_sat : int;
    nb_unsat : int;
    nb_timeout : int;
    nb_spaceout : int;
    avg_time : float
}

let get_stats h =
    let avg = ref 0. in
    let sat = ref 0 in
    let unsat = ref 0 in
    let timeout = ref 0 in
    let spaceout = ref 0 in
    let nb_success () =
        float (!sat + !unsat)
    in
    let update_avg t =
        let n = nb_success () in
        avg := (!avg *. n +. t) /. (n +. 1.)
    in
    let aux f pb = Parselog.(
        match pb.pb_st with
        | Sat ->
                update_avg pb.pb_time;
                incr sat
        | Unsat ->
                update_avg pb.pb_time;
                incr unsat
        | Timeout ->
                incr timeout
        | Spaceout ->
                incr spaceout
    ) in
    Hashtbl.iter aux h;
    {
        nb_sat = !sat;
        nb_unsat = !unsat;
        nb_timeout = !timeout;
        nb_spaceout = !spaceout;
        avg_time = !avg;
    }

(* Diff computing *)
type diff = {
    sat_limit : string list;
    sat_nopb : string list;
    unsat_limit : string list;
    unsat_nopb : string list;
    disagree : (Parselog.pb * Parselog.pb) list;
}

let empty_diff = {
    sat_limit = [];
    sat_nopb = [];
    unsat_limit = [];
    unsat_nopb = [];
    disagree = [];
}

let diff h h' =
    let aux f pb acc =
        try
            let pb' = Hashtbl.find h' f in
            Parselog.(match pb.pb_st, pb'.pb_st with
            | Sat, Timeout | Sat, Spaceout ->
                    { acc with sat_limit = f :: acc.sat_limit }
            | Unsat, Timeout | Unsat, Spaceout ->
                    { acc with unsat_limit = f :: acc.unsat_limit }
            | Sat, Unsat | Unsat, Sat ->
                    { acc with disagree = (pb, pb') :: acc.disagree }
            | _, _ -> acc
            )
        with Not_found ->
            match Parselog.(pb.pb_st) with
            | Parselog.Sat -> { acc with sat_nopb = f :: acc.sat_nopb }
            | Parselog.Unsat -> { acc with unsat_nopb = f :: acc.unsat_nopb }
            | _ -> acc
    in
    Hashtbl.fold aux h empty_diff

(* Printing *)
let print_stats s =
    let sha, h = get_commit s in
    let st = get_stats h in
    Format.printf "%s@\nAverage time : %f (%d / %d)@\nTimeouts : %d@\nSpaceouts : %d@."
    (if !info_commit then Parselog.commit_info sha else sha) st.avg_time (st.nb_sat + st.nb_unsat)
    (st.nb_sat + st.nb_unsat + st.nb_timeout + st.nb_spaceout) st.nb_timeout st.nb_spaceout

let print_diff_short s1 s2 =
    let sha1, h1 = get_commit s1 in
    let sha2, h2 = get_commit s2 in
    let only1 = diff h1 h2 in
    let only2 = diff h2 h1 in
    Format.printf "1:%s -> 2:%s@\n" sha1 sha2;
    Format.printf "Lost (only in 1) : %d (+ %d not in pb list)@\n"
    (List.length only1.sat_limit + List.length only1.unsat_limit) (List.length only1.sat_nopb + List.length only1.unsat_nopb);
    Format.printf "Won  (only in 2) : %d (+ %d not in pb list)@\n"
    (List.length only2.sat_limit + List.length only2.unsat_limit) (List.length only2.sat_nopb + List.length only2.unsat_nopb);
    match List.sort_uniq Pervasives.compare (List.rev_append only1.disagree only2.disagree) with
    | [] -> ()
    | l -> Format.printf "WARNING : %d incoherence@\n" (List.length l)

(* Main function *)
let main () =
    Arg.parse args anon usage;
    match List.rev (!arg_commit) with
    | [] -> print_stats (Parselog.last_commit ())
    | [c] -> print_stats c
    | [c1; c2] -> print_diff_short c1 c2
    | _ -> Arg.usage args usage
;;

try
    main ()
with
| Parselog.Unknown_status (f, l) ->
        Format.printf "For file '%s' : unknown return string :@\n" f;
        List.iter (fun s -> Format.printf "%s@." s) l;
        exit 3
| Commit_not_found ->
        Format.printf "No such commit found@.";
        exit 2
| Commit_ambiguous ->
        Format.printf "Too many commits fit this prefix@.";
        exit 2
