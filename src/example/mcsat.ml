(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module Fsmt = Expr

module Tsmt = struct

  module M = Map.Make(Fsmt.Term)
  module CC = Cc.Make(String)

  (* Type definitions *)
  type term = Fsmt.Term.t
  type formula = Fsmt.t
  type proof = unit

  type level = {
    cc : CC.t;
    assign : (term * int) M.t;
  }

  (* Functions *)
  let dummy = { cc = CC.empty; assign = M.empty; }

  let env = ref dummy

  let current_level () = !env

  let to_clause (a, b, l) =
    Log.debugf 10 "@[<2>Expl : %s; %s@ %a@]"
      (fun k->k a b
        (fun out () -> List.iter (Format.fprintf out " |- %s@ ") l) ());
    let rec aux acc = function
      | [] | [_] -> acc
      | x :: ((y :: _) as r) ->
        aux (Fsmt.mk_eq x y :: acc) r
    in
    (Fsmt.mk_eq a b) :: (List.rev_map Fsmt.neg (aux [] l))

  let assume s =
    let open Plugin_intf in
    try
      for i = s.start to s.start + s.length - 1 do
        match s.get i with
        | Assign (x, v, lvl) ->
          env := { !env with assign = M.add x (v, lvl) !env.assign }
        | Lit f ->
          Log.debugf 10 "Propagating in th :@ @[%a@]" (fun k->k Fsmt.print f);
          match f with
          | Fsmt.Prop _ -> ()
          | Fsmt.Equal (i, j) ->
            env := { !env with cc = CC.add_eq !env.cc i j }
          | Fsmt.Distinct (i, j) ->
            env := { !env with cc = CC.add_neq !env.cc i j }
      done;
      Sat
    with CC.Unsat x ->
      Log.debug 8 "Making explanation clause...";
      Unsat (to_clause x, ())

  let backtrack l = env := l

  let assign t = CC.repr !env.cc t

  let iter_assignable f = function
    | Fsmt.Prop _ -> ()
    | Fsmt.Equal(a, b)
    | Fsmt.Distinct (a, b) -> f a; f b

  let max (a: int) (b: int) = if a < b then b else a

  let eval = function
    | Fsmt.Prop _ -> Plugin_intf.Unknown
    | Fsmt.Equal (a, b) ->
      begin try
          let a', lvl_a = M.find a !env.assign in
          let b', lvl_b = M.find b !env.assign in
          Plugin_intf.Valued (Fsmt.Term.equal a' b', max lvl_a lvl_b)
        with Not_found ->
          Plugin_intf.Unknown
      end
    | Fsmt.Distinct (a, b) ->
      begin try
          let a', lvl_a = M.find a !env.assign in
          let b', lvl_b = M.find b !env.assign in
          Plugin_intf.Valued (not (Fsmt.Term.equal a' b'), max lvl_a lvl_b)
        with Not_found ->
          Plugin_intf.Unknown
      end

  let if_sat _ =
    Plugin_intf.Sat

end

module Make(Dummy:sig end) = struct

  include Mcsolver.Make(Fsmt)(Tsmt)(struct end)
  module Dot = Dot.Make(Proof)(struct
      let print_atom = St.print_atom
      let lemma_info () = "Proof", Some "PURPLE", []
    end)
  module Dedukti = Dedukti.Make(Proof)(struct
      let print _ _ = ()
      let prove _ _ = ()
      let context _ _ = ()
    end)

  let print_clause = St.print_clause
  let print_dot = Dot.print
  let print_dedukti = Dedukti.print

end
