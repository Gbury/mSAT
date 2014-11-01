
module S = Sat.Make(struct end)

(* Arguments parsing *)
let file = ref ""

let input_file = fun s -> file := s
let usage = "Usage : main [options] <file>"
let argspec = Arg.align [
    "-v", Arg.Int (fun i -> Log.set_debug i),
    "<lvl> Sets the debug verbose level";
]

(* Entry file parsing *)
let get_cnf () =
    let chan = open_in !file in
    let lexbuf = Lexing.from_channel chan in
    let l = Parsedimacs.file Lexdimacs.token lexbuf in
    List.map (List.map S.make) l

let print_cnf cnf =
    Format.printf "CNF :@\n";
    List.iter (fun c ->
        Format.fprintf Format.std_formatter "%a;@\n"
        (fun fmt -> List.iter (fun a ->
            Format.fprintf fmt "%a@ " S.print_atom a
            )
        ) c
    ) cnf

(* Iterate over all variables created *)
let print_assign fmt () =
  S.iter_atoms (fun a ->
      Format.fprintf fmt "%a -> %s,@ "
        S.print_atom a
        (if S.eval a then "true" else "false")
    )

let main () =
  Arg.parse argspec input_file usage;
  if !file = "" then begin
      Arg.usage argspec usage;
      exit 2
  end;
  let cnf = get_cnf () in
  print_cnf cnf;
  ()
;;

main ()
