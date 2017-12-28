
(* Module initialization *)

open Msat_smt

module E = Eclosure.Make(Expr_smt.Term)
module H = Backtrack.Hashtbl(Expr_smt.Term)
module M = Hashtbl.Make(Expr_smt.Term)

(* Type definitions *)

type proof = unit
type term = Expr_smt.Term.t
type formula = Expr_smt.Atom.t
type level = Backtrack.Stack.level

exception Absurd of Expr_smt.Atom.t list

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

(* Propositional constants *)

let true_ = Expr_smt.(Term.of_id (Id.ty "true" Ty.prop))
let false_ = Expr_smt.(Term.of_id (Id.ty "false" Ty.prop))

(* Uninterpreted functions and predicates *)

let map : Expr_smt.term H.t = H.create stack
let watch = M.create 4096
let interpretation = H.create stack

let pop_watches t =
  try
    let l = M.find watch t in
    M.remove watch t;
    l
  with Not_found ->
    []

let add_job j x =
  let l = try M.find watch x with Not_found -> [] in
  M.add watch x (j :: l)

let update_job x ((t, watchees) as job) =
  try
    let y = List.find (fun y -> not (H.mem map y)) watchees in
    add_job job y
  with Not_found ->
    add_job job x;
    begin match t with
      | { Expr_smt.term = Expr_smt.App (f, tys, l);_ } ->
        let is_prop = Expr_smt.(Ty.equal t.t_type Ty.prop) in
        let t_v = H.find map t in
        let l' = List.map (H.find map) l in
        let u = Expr_smt.Term.apply f tys l' in
        begin try
            let t', u_v = H.find interpretation u in
            if not (Expr_smt.Term.equal t_v u_v) then begin
              match t' with
              | { Expr_smt.term = Expr_smt.App (_, _, r); _ } when is_prop ->
                let eqs = List.map2 (fun a b -> Expr_smt.Atom.neg (Expr_smt.Atom.eq a b)) l r in
                if Expr_smt.(Term.equal u_v true_) then begin
                  let res = Expr_smt.Atom.pred t ::
                            Expr_smt.Atom.neg (Expr_smt.Atom.pred t') :: eqs in
                  raise (Absurd res)
                end else begin
                  let res = Expr_smt.Atom.pred t' ::
                            Expr_smt.Atom.neg (Expr_smt.Atom.pred t) :: eqs in
                  raise (Absurd res)
                end
              | { Expr_smt.term = Expr_smt.App (_, _, r); _ } ->
                let eqs = List.map2 (fun a b -> Expr_smt.Atom.neg (Expr_smt.Atom.eq a b)) l r in
                let res = Expr_smt.Atom.eq t t' :: eqs in
                raise (Absurd res)
              | _ -> assert false
            end
          with Not_found ->
            H.add interpretation u (t, t_v);
        end
      | _ -> assert false
    end

let rec update_watches x = function
  | [] -> ()
  | job :: r ->
    begin
      try
        update_job x job;
      with exn ->
        List.iter (fun j -> add_job j x) r;
        raise exn
    end;
    update_watches x r

let add_watch t l =
  update_job t (t, l)

let add_assign t v =
  H.add map t v;
  update_watches t (pop_watches t)

(* Assignemnts *)

let rec iter_aux f = function
  | { Expr_smt.term = Expr_smt.Var _; _ } as t ->
    Log.debugf 10 (fun k -> k "Adding %a as assignable" Expr_smt.Term.print t);
    f t
  | { Expr_smt.term = Expr_smt.App (_, _, l); _ } as t ->
    if l <> [] then add_watch t (t :: l);
    List.iter (iter_aux f) l;
    Log.debugf 10 (fun k -> k "Adding %a as assignable" Expr_smt.Term.print t);
    f t

let iter_assignable f = function
  | { Expr_smt.atom = Expr_smt.Pred { Expr_smt.term = Expr_smt.Var _;_ }; _ } -> ()
  | { Expr_smt.atom = Expr_smt.Pred ({ Expr_smt.term = Expr_smt.App (_, _, l);_} as t); _ } ->
    if l <> [] then add_watch t (t :: l);
    List.iter (iter_aux f) l;
  | { Expr_smt.atom = Expr_smt.Equal (a, b);_ } ->
    iter_aux f a; iter_aux f b

let eval = function
  | { Expr_smt.atom = Expr_smt.Pred t; _ } ->
    begin try
        let v = H.find map t in
        if Expr_smt.Term.equal v true_ then
          Plugin_intf.Valued (true, [t])
        else if Expr_smt.Term.equal v false_ then
          Plugin_intf.Valued (false, [t])
        else
          Plugin_intf.Unknown
      with Not_found ->
        Plugin_intf.Unknown
    end
  | { Expr_smt.atom = Expr_smt.Equal (a, b); sign; _ } ->
    begin try
        let v_a = H.find map a in
        let v_b = H.find map b in
        if Expr_smt.Term.equal v_a v_b then
          Plugin_intf.Valued(sign, [a; b])
        else
          Plugin_intf.Valued(not sign, [a; b])
      with Not_found ->
        Plugin_intf.Unknown
    end


(* Theory propagation *)

let rec chain_eq = function
  | [] | [_] -> []
  | a :: ((b :: _) as l) -> (Expr_smt.Atom.eq a b) :: chain_eq l

let assume s =
  let open Plugin_intf in
  try
    for i = s.start to s.start + s.length - 1 do
      match s.get i with
      | Assign (t, v) ->
        add_assign t v;
        E.add_tag uf t v
      | Lit f ->
        begin match f with
          | { Expr_smt.atom = Expr_smt.Equal (u, v); sign = true;_ } ->
            E.add_eq uf u v
          | { Expr_smt.atom = Expr_smt.Equal (u, v); sign = false;_ } ->
            E.add_neq uf u v
          | { Expr_smt.atom = Expr_smt.Pred p; sign;_ } ->
            let v = if sign then true_ else false_ in
            add_assign p v
        end
    done;
    Plugin_intf.Sat
  with
  | Absurd l ->
    Plugin_intf.Unsat (l, ())
  | E.Unsat (a, b, l) ->
    let c = Expr_smt.Atom.eq a b :: List.map Expr_smt.Atom.neg (chain_eq l) in
    Plugin_intf.Unsat (c, ())

let if_sat _ =
  Plugin_intf.Sat

