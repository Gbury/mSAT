
(** {1 simple sudoku solver} *)

open Sudoku_solver


let solve_file file =
  Format.printf "solve grids in file %S@." file;
  let start = Sys.time() in
  let grids =
    CCIO.with_in file CCIO.read_lines_l
    |> CCList.filter_map
      (fun s ->
         let s = String.trim s in
         if s="" then None
         else match Grid.parse s with
           | g -> Some g
           | exception e ->
             errorf "cannot parse sudoku %S: %s@." s (Printexc.to_string e))
  in
  Format.printf "parsed %d grids (in %.3fs)@." (List.length grids) (Sys.time()-.start);
  List.iter
    (fun g ->
       Format.printf "@[<v>@,#########################@,@[<2>solve grid:@ %a@]@]@." Grid.pp g;
       let start=Sys.time() in
       match solve_grid g with
       | None -> Format.printf "no solution (in %.3fs)@." (Sys.time()-.start)
       | Some g' when not @@ Grid.is_full g' ->
         errorf "grid %a@ is not full" Grid.pp g'
       | Some g' when not @@ Grid.is_valid g' ->
         errorf "grid %a@ is not valid" Grid.pp g'
       | Some g' when not @@ Grid.matches ~pat:g g' ->
         errorf "grid %a@ @[<2>does not match original@ %a@]" Grid.pp g' Grid.pp g
       | Some g' ->
         Format.printf "@[<v>@[<2>solution (in %.3fs):@ %a@]@,###################@]@."
           (Sys.time()-.start) Grid.pp g')
    grids

let () =
  Fmt.set_color_default true;
  let files = ref [] in
  let debug = ref 0 in
  let opts = [
    "--debug", Arg.Set_int debug, " debug";
    "-d", Arg.Set_int debug, " debug";
  ] |> Arg.align in
  Arg.parse opts (fun f -> files := f :: !files) "sudoku_solve [options] <file>";
  Msat.Log.set_debug !debug;
  try
    List.iter (fun f -> solve_file f) !files;
  with
  | Failure msg | Invalid_argument msg ->
    Format.printf "@{<Red>Error@}:@.%s@." msg;
    exit 1
