
module S = Sat.Make(struct end)

let anon_fun = fun _ -> ()
let usage = "Coming soon..."
let argspec = [
    "-v", Arg.Int (fun i -> Log.set_debug i),
    "Sets the debug verbose level";
]

(* Temp until lexer/parsezr is set up *)
let init () =
  let v = Array.init 5 (fun _ -> S.new_atom ()) in
  [ [
    [v.(0); v.(1)];
    [S.neg v.(0); v.(2)];
    [S.neg v.(1); v.(2)];
    [v.(3); v.(4)];
    [S.neg v.(3); S.neg v.(2)];
    [S.neg v.(4); S.neg v.(2)];
    ]
  ]

(* Iterate over all variables created *)
let print_assign fmt () =
  S.iter_atoms (fun a ->
      Format.fprintf fmt "%a -> %s,@ "
        S.print_atom a
        (if S.eval a then "true" else "false")
    )

let main () =
  Arg.parse argspec anon_fun usage;
  List.iter (fun l ->
      List.iter (fun c -> Format.printf "Adding : %a@."
                    (fun _ -> List.iter (fun a -> Format.printf "%a " S.print_atom a)) c) l;
      S.assume l;
      match S.solve () with
      | S.Sat -> Format.printf "Sat@\n%a@." print_assign ()
      | S.Unsat -> Format.printf "Unsat@.") (init ());
;;

main ()
