(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

exception Commit_not_found
exception Commit_ambiguous

let get_commit s =
    let d = Filename.concat (Filename.dirname Sys.argv.(0)) "bench" in
    match Parselog.complete (Filename.concat d "logs") s with
    | [c] -> Filename.basename c, Parselog.parse_commit c
    | [] -> raise Commit_not_found
    | _ -> raise Commit_ambiguous

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

let print_stats s =
    let sha, h = get_commit s in
    let st = get_stats h in
    Format.printf "Average time : %f (%d / %d)@\nTimeouts : %d@\nSpaceouts : %d@."
    st.avg_time (st.nb_sat + st.nb_unsat)
    (st.nb_sat + st.nb_unsat + st.nb_timeout + st.nb_spaceout) st.nb_timeout st.nb_spaceout

let print_diff _ _ = ()

let main () =
    if Array.length Sys.argv = 2 then
        print_stats Sys.argv.(1)
    else if Array.length Sys.argv = 3 then
        print_diff Sys.argv.(1) Sys.argv.(2)
    else
        Format.fprintf Format.std_formatter "Don't know what to do !@."
;;

try
    main ()
with
| Commit_not_found ->
        Format.printf "No such commit found@.";
        exit 2
| Commit_ambiguous ->
        Format.printf "Too many commits fit this prefix@.";
        exit 2
