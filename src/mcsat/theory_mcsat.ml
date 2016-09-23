
(* Module initialization *)

module E = Eclosure.Make(Expr_smt.Term)
module H = Backtrack.Hashtbl(Expr_smt.Term)

(* Type definitions *)

type proof = unit
type term = Expr_smt.Term.t
type formula = Expr_smt.Atom.t

type level = Backtrack.Stack.level


(* Backtracking *)

let stack = Backtrack.Stack.create ()

let dummy = Backtrack.Stack.dummy_level

let current_level () = Backtrack.Stack.level stack

let backtrack = Backtrack.Stack.backtrack stack

(* Equality closure *)

let uf = E.create stack

let assign t =
  match E.find_tag uf t with
  | _, None -> t
  | _, Some (_, v) -> v

(* Uninterpreted functions and predicates *)

let map = H.create stack

let true_ = Expr_smt.(Term.of_id (Id.ty "true" Ty.prop))
let false_ = Expr_smt.(Term.of_id (Id.ty "false" Ty.prop))

let add_assign t v lvl =
  H.add map t (v, lvl)

(* Assignemnts *)

let rec iter_aux f = function
  | { Expr_smt.term = Expr_smt.Var _ } as t ->
    f t
  | { Expr_smt.term = Expr_smt.App (_, _, l) } as t ->
    List.iter (iter_aux f) l;
    f t

let iter_assignable f = function
  | { Expr_smt.atom = Expr_smt.Pred { Expr_smt.term = Expr_smt.Var _ } } -> ()
  | { Expr_smt.atom = Expr_smt.Pred { Expr_smt.term = Expr_smt.App (_, _, l) } } ->
    List.iter (iter_aux f) l;
  | { Expr_smt.atom = Expr_smt.Equal (a, b) } ->
    iter_aux f a; iter_aux f b

let eval = function
  | { Expr_smt.atom = Expr_smt.Pred t } ->
    begin try
        let v, lvl = H.find map t in
        if Expr_smt.Term.equal v true_ then
          Plugin_intf.Valued (true, lvl)
        else if Expr_smt.Term.equal v false_ then
          Plugin_intf.Valued (false, lvl)
        else
          Plugin_intf.Unknown
      with Not_found ->
        Plugin_intf.Unknown
    end
  | { Expr_smt.atom = Expr_smt.Equal (a, b); sign } ->
    begin try
        let v_a, a_lvl = H.find map a in
        let v_b, b_lvl = H.find map a in
        if Expr_smt.Term.equal v_a v_b then
          Plugin_intf.Valued(sign, max a_lvl b_lvl)
        else
          Plugin_intf.Valued(not sign, max a_lvl b_lvl)
      with Not_found ->
        Plugin_intf.Unknown
    end


(* Theory propagation *)

let if_sat _ = ()

let rec chain_eq = function
  | [] | [_] -> []
  | a :: ((b :: r) as l) -> (Expr_smt.Atom.eq a b) :: chain_eq l

let assume s =
  let open Plugin_intf in
  try
    for i = s.start to s.start + s.length - 1 do
      match s.get i with
      | Assign (t, v, lvl) ->
        add_assign t v lvl;
        E.add_tag uf t v
      | Lit f ->
        begin match f with
          | { Expr_smt.atom = Expr_smt.Equal (u, v); sign = true } ->
            E.add_eq uf u v
          | { Expr_smt.atom = Expr_smt.Equal (u, v); sign = false } ->
            E.add_neq uf u v
          | { Expr_smt.atom = Expr_smt.Pred p; sign } ->
            let v = if sign then true_ else false_ in
            add_assign p v ~-1
        end
    done;
    Plugin_intf.Sat
  with
  | E.Unsat (a, b, l) ->
    let c = Expr_smt.Atom.eq a b :: List.map Expr_smt.Atom.neg (chain_eq l) in
    Plugin_intf.Unsat (c, ())

