
open Js_of_ocaml

let solve_ (s:string) : string =
  let open Sudoku_solver in
  let g = match Grid.parse s with
  | g -> g
  | exception e ->
    let msg = Printf.sprintf "cannot parse sudoku %S: %s" s (Printexc.to_string e) in
    let err = new%js Js.error_constr (Js.string msg) in
    Js.raise_js_error err
  in
  let solver = Solver.create g in
  match Solver.solve solver with
  | None -> ""
  | Some g' -> Grid.serialize g'

let () =
  let o = object%js
    method sudokuSolve s =
      let s = Js.to_string s in
      solve_ s |> Js.string
  end in
  Js.export_all o
