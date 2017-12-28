(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

(* Log&Module Init *)
(* ************************************************************************ *)

module Id = Dolmen.Id
module Ast = Dolmen.Term
module H = Hashtbl.Make(Id)
module Formula = Msat_tseitin.Make(Expr_sat)

(* Exceptions *)
(* ************************************************************************ *)

exception Typing_error of string * Dolmen.Term.t

(* Identifiers *)
(* ************************************************************************ *)

let symbols = H.create 42

let find_id id =
  try
    H.find symbols id
  with Not_found ->
    let res = Expr_sat.fresh () in
    H.add symbols id res;
    res

(* Actual parsing *)
(* ************************************************************************ *)

[@@@ocaml.warning "-9"]

let rec parse = function
  | { Ast.term = Ast.Builtin Ast.True } ->
    Formula.f_true
  | { Ast.term = Ast.Builtin Ast.False } ->
    Formula.f_false
  | { Ast.term = Ast.Symbol id } ->
    let s = find_id id in
    Formula.make_atom s
  | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Not }, [p]) }
  | { Ast.term = Ast.App ({Ast.term = Ast.Symbol { Id.name = "not" } }, [p]) } ->
    Formula.make_not (parse p)
  | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.And }, l) }
  | { Ast.term = Ast.App ({Ast.term = Ast.Symbol { Id.name = "and" } }, l) } ->
    Formula.make_and (List.map parse l)
  | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Or }, l) }
  | { Ast.term = Ast.App ({Ast.term = Ast.Symbol { Id.name = "or" } }, l) } ->
    Formula.make_or (List.map parse l)
  | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Imply }, [p; q]) } ->
    Formula.make_imply (parse p) (parse q)
  | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Equiv }, [p; q]) } ->
    Formula.make_equiv (parse p) (parse q)
  | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Xor }, [p; q]) } ->
    Formula.make_xor (parse p) (parse q)
  | t ->
    raise (Typing_error ("Term is not a pure proposition", t))

[@@@ocaml.warning "+9"]

(* Exported functions *)
(* ************************************************************************ *)

let decl _ t =
  raise (Typing_error ("Declarations are not allowed in pure sat", t))

let def _ t =
  raise (Typing_error ("Definitions are not allowed in pure sat", t))

let assumptions t =
  let f = parse t in
  let cnf = Formula.make_cnf f in
  List.map (function
      | [ x ] -> x
      | _ -> assert false
    ) cnf

let antecedent t =
  let f = parse t in
  Formula.make_cnf f

let consequent t =
  let f = parse t in
  Formula.make_cnf @@ Formula.make_not f

