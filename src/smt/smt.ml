(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module Th = Solver.DummyTheory(Expr_smt.Atom)

  (* struct
  module CC = Cc.Make(String)

  type formula = Fsmt.t
  type proof = unit
  type level = CC.t

  let dummy = CC.empty

  let env = ref dummy

  let current_level () = !env

  let to_clause (a, b, l) =
    Log.debugf 10 "@[Expl : %s; %s@ %a@]"
      (fun k->k a b
        (fun out () -> List.iter (Format.fprintf out "|- %s@ ") l) ());
    let rec aux acc = function
      | [] | [_] -> acc
      | x :: ((y :: _) as r) ->
        aux (Fsmt.mk_eq x y :: acc) r
    in
    (Fsmt.mk_eq a b) :: (List.rev_map Fsmt.neg (aux [] l))

  let assume s =
    let open Theory_intf in
    try
      for i = s.start to s.start + s.length - 1 do
        Log.debugf 10 "Propagating in th :@ @[%a@]" (fun k->k Fsmt.print (s.get i));
        match s.get i with
        | Fsmt.Prop _ -> ()
        | Fsmt.Equal (i, j) -> env := CC.add_eq !env i j
        | Fsmt.Distinct (i, j) -> env := CC.add_neq !env i j
      done;
      Sat
    with CC.Unsat x ->
      Log.debug 8 "Making explanation clause...";
      Unsat (to_clause x, ())

  let backtrack l = env := l

  let if_sat _ = ()

end
  *)

module Make(Dummy:sig end) =
  Solver.Make(Expr_smt.Atom)(Th)(struct end)
