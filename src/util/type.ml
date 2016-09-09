
(* Log&Module Init *)
(* ************************************************************************ *)

module Ast = Dolmen.Term
module Id = Dolmen.Id
module M = Map.Make(Id)
module H = Hashtbl.Make(Id)

(* Types *)
(* ************************************************************************ *)

(* The type of potentially expected result type for parsing an expression *)
type expect =
  | Nothing
  | Type
  | Typed of Msat.Expr.ty

(* The type returned after parsing an expression. *)
type res =
  | Ttype
  | Ty of Msat.Expr.ty
  | Term of Msat.Expr.term
  | Formula of Msat.Expr.Formula.t


(* The local environments used for type-checking. *)
type env = {

  (* local variables (mostly quantified variables) *)
  type_vars : (Msat.Expr.ttype Msat.Expr.id)  M.t;
  term_vars : (Msat.Expr.ty Msat.Expr.id)     M.t;

  (* Bound variables (through let constructions) *)
  term_lets : Msat.Expr.term     M.t;
  prop_lets : Msat.Expr.Formula.t M.t;

  (* Typing options *)
  expect   : expect;
}

(* Exceptions *)
(* ************************************************************************ *)

(* Internal exception *)
exception Found of Ast.t

(* Exception for typing errors *)
exception Typing_error of string * Ast.t

(* Convenience functions *)
let _expected s t = raise (Typing_error (
    Format.asprintf "Expected a %s" s, t))
let _bad_arity s n t = raise (Typing_error (
    Format.asprintf "Bad arity for operator '%s' (expected %d arguments)" s n, t))
let _type_mismatch t ty ty' ast = raise (Typing_error (
    Format.asprintf "Type Mismatch: '%a' has type %a, but an expression of type %a was expected"
      Msat.Expr.Print.term t Msat.Expr.Print.ty ty Msat.Expr.Print.ty ty', ast))
let _fo_term s t = raise (Typing_error (
    Format.asprintf "Let-bound variable '%a' is applied to terms" Id.print s, t))

(* Global Environment *)
(* ************************************************************************ *)

(* Global identifier table; stores declared types and aliases. *)
let global_env = H.create 42

let find_global name =
  try H.find global_env name
  with Not_found -> `Not_found

(* Symbol declarations *)
let decl_ty_cstr id c =
  if H.mem global_env id then
    Msat.Log.debugf 0 "Symbol '%a' has already been defined, overwriting previous definition"
      (fun k -> k Id.print id);
  H.add global_env id (`Ty c);
  Msat.Log.debugf 1 "New type constructor : %a" (fun k -> k Msat.Expr.Print.const_ttype c)

let decl_term id c =
  if H.mem global_env id then
    Msat.Log.debugf 0 "Symbol '%a' has already been defined, overwriting previous definition"
      (fun k -> k Id.print id);
  H.add global_env id (`Term c);
  Msat.Log.debugf 1 "New constant : %a" (fun k -> k Msat.Expr.Print.const_ty c)

(* Symbol definitions *)
let def_ty id args body =
  if H.mem global_env id then
    Msat.Log.debugf 0 "Symbol '%a' has already been defined, overwriting previous definition"
      (fun k -> k Id.print id);
  H.add global_env id (`Ty_alias (args, body))

let def_term id ty_args args body =
  if H.mem global_env id then
    Msat.Log.debugf 0 "Symbol '%a' has already been defined, overwriting previous definition"
      (fun k -> k Id.print id);
  H.add global_env id (`Term_alias (ty_args, args, body))

(* Local Environment *)
(* ************************************************************************ *)

(* Make a new empty environment *)
let empty_env ?(expect=Nothing) () = {
  type_vars = M.empty;
  term_vars = M.empty;
  term_lets = M.empty;
  prop_lets = M.empty;
  expect;
}

(* Generate new fresh names for shadowed variables *)
let new_name pre =
  let i = ref 0 in
  (fun () -> incr i; pre ^ (string_of_int !i))

let new_ty_name = new_name "ty#"
let new_term_name = new_name "term#"

(* Add local variables to environment *)
let add_type_var env id v =
  let v' =
    if M.mem id env.type_vars then
      Msat.Expr.Id.ttype (new_ty_name ())
    else
      v
  in
  Msat.Log.debugf 1 "New binding : %a -> %a"
    (fun k -> k Id.print id Msat.Expr.Print.id_ttype v');
  v', { env with type_vars = M.add id v' env.type_vars }

let add_type_vars env l =
  let l', env' = List.fold_left (fun (l, acc) (id, v) ->
      let v', acc' = add_type_var acc id v in
      v' :: l, acc') ([], env) l in
  List.rev l', env'

let add_term_var env id v =
  let v' =
    if M.mem id env.type_vars then
      Msat.Expr.Id.ty (new_term_name ()) Msat.Expr.(v.id_type)
    else
      v
  in
  Msat.Log.debugf 1 "New binding : %a -> %a"
    (fun k -> k Id.print id Msat.Expr.Print.id_ty v');
  v', { env with term_vars = M.add id v' env.term_vars }

let find_var env name =
  try `Ty (M.find name env.type_vars)
  with Not_found ->
    begin
      try
        `Term (M.find name env.term_vars)
      with Not_found ->
        `Not_found
    end

(* Add local bound variables to env *)
let add_let_term env id t =
  Msat.Log.debugf 1 "New let-binding : %s -> %a"
    (fun k -> k id.Id.name Msat.Expr.Print.term t);
  { env with term_lets = M.add id t env.term_lets }

let add_let_prop env id t =
  Msat.Log.debugf 1 "New let-binding : %s -> %a"
    (fun k -> k id.Id.name Msat.Expr.Formula.print t);
  { env with prop_lets = M.add id t env.prop_lets }

let find_let env name =
  try `Term (M.find name env.term_lets)
  with Not_found ->
    begin
      try
        `Prop (M.find name env.prop_lets)
      with Not_found ->
        `Not_found
    end

let pp_expect fmt = function
  | Nothing -> Format.fprintf fmt "<>"
  | Type -> Format.fprintf fmt "<tType>"
  | Typed ty -> Msat.Expr.Print.ty fmt ty

let pp_map pp fmt map =
  M.iter (fun k v ->
      Format.fprintf fmt "%s->%a;" k.Id.name pp v) map

(* Some helper functions *)
(* ************************************************************************ *)

let flat_map f l = List.flatten (List.map f l)

let take_drop n l =
  let rec aux acc = function
    | 0, _ | _, [] -> List.rev acc, []
    | m, x :: r -> aux (x :: acc) (m - 1, r)
  in
  aux [] (n, l)

let diagonal l =
  let rec single x acc = function
    | [] -> acc
    | y :: r -> single x ((x, y) :: acc) r
  and aux acc = function
    | [] -> acc
    | x :: r -> aux (single x acc r) r
  in
  aux [] l

(* Wrappers for expression building *)
(* ************************************************************************ *)

let arity f =
  List.length Msat.Expr.(f.id_type.fun_vars) +
  List.length Msat.Expr.(f.id_type.fun_args)

let ty_apply env ast f args =
  try
    Msat.Expr.Ty.apply f args
  with Msat.Expr.Bad_ty_arity _ ->
    _bad_arity Msat.Expr.(f.id_name) (arity f) ast

let term_apply env ast f ty_args t_args =
  try
    Msat.Expr.Term.apply f ty_args t_args
  with
  | Msat.Expr.Bad_arity _ ->
    _bad_arity Msat.Expr.(f.id_name) (arity f) ast
  | Msat.Expr.Type_mismatch (t, ty, ty') ->
    _type_mismatch t ty ty' ast

let ty_subst ast_term id args f_args body =
  let aux s v ty = Msat.Expr.Subst.Id.bind v ty s in
  match List.fold_left2 aux Msat.Expr.Subst.empty f_args args with
  | subst ->
    Msat.Expr.Ty.subst subst body
  | exception Invalid_argument _ ->
    _bad_arity id.Id.name (List.length f_args) ast_term

let term_subst ast_term id ty_args t_args f_ty_args f_t_args body =
  let aux s v ty = Msat.Expr.Subst.Id.bind v ty s in
  match List.fold_left2 aux Msat.Expr.Subst.empty f_ty_args ty_args with
  | ty_subst ->
    begin
      let aux s v t = Msat.Expr.Subst.Id.bind v t s in
      match List.fold_left2 aux Msat.Expr.Subst.empty f_t_args t_args with
      | t_subst ->
        Msat.Expr.Term.subst ty_subst t_subst body
      | exception Invalid_argument _ ->
        _bad_arity id.Id.name (List.length f_ty_args + List.length f_t_args) ast_term
    end
  | exception Invalid_argument _ ->
    _bad_arity id.Id.name (List.length f_ty_args + List.length f_t_args) ast_term

let make_eq ast_term a b =
  try
    Msat.Expr.Formula.make_atom @@ Msat.Expr.Atom.eq a b
  with Msat.Expr.Type_mismatch (t, ty, ty') ->
    _type_mismatch t ty ty' ast_term

let make_pred ast_term p =
  try
    Msat.Expr.Formula.make_atom @@ Msat.Expr.Atom.pred p
  with Msat.Expr.Type_mismatch (t, ty, ty') ->
    _type_mismatch t ty ty' ast_term

let infer env s args =
  match env.expect with
  | Nothing -> `Nothing
  | Type ->
    let n = List.length args in
    let res = Msat.Expr.Id.ty_fun s.Id.name n in
    decl_ty_cstr s res;
    `Ty res
  | Typed ty ->
    let n = List.length args in
    let rec replicate acc n =
      if n <= 0 then acc else replicate (Msat.Expr.Ty.base :: acc) (n - 1)
    in
    let res = Msat.Expr.Id.term_fun s.Id.name [] (replicate [] n) ty in
    decl_term s res;
    `Term res

(* Msat.Expression parsing *)
(* ************************************************************************ *)

let rec parse_expr (env : env) t =
  match t with

  (* Basic formulas *)
  | { Ast.term = Ast.App ({ Ast.term = Ast.Builtin Ast.True }, []) }
  | { Ast.term = Ast.Builtin Ast.True } ->
    Formula Msat.Expr.Formula.f_true

  | { Ast.term = Ast.App ({ Ast.term = Ast.Builtin Ast.False }, []) }
  | { Ast.term = Ast.Builtin Ast.False } ->
    Formula Msat.Expr.Formula.f_false

  | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.And}, l) } ->
    Formula (Msat.Expr.Formula.make_and (List.map (parse_formula env) l))

  | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Or}, l) } ->
    Formula (Msat.Expr.Formula.make_or (List.map (parse_formula env) l))

  | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Xor}, l) } as t ->
    begin match l with
      | [p; q] ->
        let f = parse_formula env p in
        let g = parse_formula env q in
        Formula (Msat.Expr.Formula.make_not (Msat.Expr.Formula.make_equiv f g))
      | _ -> _bad_arity "xor" 2 t
    end

  | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Imply}, l) } as t ->
    begin match l with
      | [p; q] ->
        let f = parse_formula env p in
        let g = parse_formula env q in
        Formula (Msat.Expr.Formula.make_imply f g)
      | _ -> _bad_arity "=>" 2 t
    end

  | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Equiv}, l) } as t ->
    begin match l with
      | [p; q] ->
        let f = parse_formula env p in
        let g = parse_formula env q in
        Formula (Msat.Expr.Formula.make_equiv f g)
      | _ -> _bad_arity "<=>" 2 t
    end

  | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Not}, l) } as t ->
    begin match l with
      | [p] ->
        Formula (Msat.Expr.Formula.make_not (parse_formula env p))
      | _ -> _bad_arity "not" 1 t
    end

  (* (Dis)Equality *)
  | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Eq}, l) } as t ->
    begin match l with
      | [a; b] ->
        Formula (
          make_eq t
            (parse_term env a)
            (parse_term env b)
        )
      | _ -> _bad_arity "=" 2 t
    end

  | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Distinct}, args) } as t ->
    let l' = List.map (parse_term env) args in
    let l'' = diagonal l' in
    Formula (
      Msat.Expr.Formula.make_and
        (List.map (fun (a, b) ->
             Msat.Expr.Formula.make_not
               (make_eq t a b)) l'')
    )

  (* General case: application *)
  | { Ast.term = Ast.Symbol s } as ast ->
    parse_app env ast s []
  | { Ast.term = Ast.App ({ Ast.term = Ast.Symbol s }, l) } as ast ->
    parse_app env ast s l

  (* Local bindings *)
  | { Ast.term = Ast.Binder (Ast.Let, vars, f) } ->
    parse_let env f vars

  (* Other cases *)
  | ast -> raise (Typing_error ("Couldn't parse the expression", ast))

and parse_var env = function
  | { Ast.term = Ast.Colon ({ Ast.term = Ast.Symbol s }, e) } ->
    begin match parse_expr env e with
      | Ttype -> `Ty (s, Msat.Expr.Id.ttype s.Id.name)
      | Ty ty -> `Term (s, Msat.Expr.Id.ty s.Id.name ty)
      | _ -> _expected "type (or Ttype)" e
    end
  | { Ast.term = Ast.Symbol s } ->
    begin match env.expect with
      | Nothing -> assert false
      | Type -> `Ty (s, Msat.Expr.Id.ttype s.Id.name)
      | Typed ty -> `Term (s, Msat.Expr.Id.ty s.Id.name ty)
    end
  | t -> _expected "(typed) variable" t

and parse_quant_vars env l =
  let ttype_vars, typed_vars, env' = List.fold_left (
      fun (l1, l2, acc) v ->
        match parse_var acc v with
        | `Ty (id, v') ->
          let v'', acc' = add_type_var acc id v' in
          (v'' :: l1, l2, acc')
        | `Term (id, v') ->
          let v'', acc' = add_term_var acc id v' in
          (l1, v'' :: l2, acc')
    ) ([], [], env) l in
  List.rev ttype_vars, List.rev typed_vars, env'

and parse_let env f = function
  | [] -> parse_expr env f
  | x :: r ->
    begin match x with
      | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Eq}, [
          { Ast.term = Ast.Symbol s }; e]) } ->
        let t = parse_term env e in
        let env' = add_let_term env s t in
        parse_let env' f r
      | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Equiv}, [
          { Ast.term = Ast.Symbol s }; e]) } ->
        let t = parse_formula env e in
        let env' = add_let_prop env s t in
        parse_let env' f r
      | { Ast.term = Ast.Colon ({ Ast.term = Ast.Symbol s }, e) } ->
        begin match parse_expr env e with
          | Term t ->
            let env' = add_let_term env s t in
            parse_let env' f r
          | Formula t ->
            let env' = add_let_prop env s t in
            parse_let env' f r
          | _ -> _expected "term of formula" e
        end
      | t -> _expected "let-binding" t
    end

and parse_app env ast s args =
  match find_let env s with
  | `Term t ->
    if args = [] then Term t
    else _fo_term s ast
  | `Prop p ->
    if args = [] then Formula p
    else _fo_term s ast
  | `Not_found ->
    begin match find_var env s with
      | `Ty f ->
        if args = [] then Ty (Msat.Expr.Ty.of_id f)
        else _fo_term s ast
      | `Term f ->
        if args = [] then Term (Msat.Expr.Term.of_id f)
        else _fo_term s ast
      | `Not_found ->
        begin match find_global s with
          | `Ty f ->
            parse_app_ty env ast f args
          | `Term f ->
            parse_app_term env ast f args
          | `Ty_alias (f_args, body) ->
            parse_app_subst_ty env ast s args f_args body
          | `Term_alias (f_ty_args, f_t_args, body) ->
            parse_app_subst_term env ast s args f_ty_args f_t_args body
          | `Not_found ->
            begin match infer env s args with
              | `Ty f -> parse_app_ty env ast f args
              | `Term f -> parse_app_term env ast f args
              | `Nothing ->
                raise (Typing_error (
                    Format.asprintf "Scoping error: '%a' not found" Id.print s, ast))
            end
        end
    end

and parse_app_ty env ast f args =
  let l = List.map (parse_ty env) args in
  Ty (ty_apply env ast f l)

and parse_app_term env ast f args =
  let n = List.length Msat.Expr.(f.id_type.fun_vars) in
  let ty_l, t_l = take_drop n args in
  let ty_args = List.map (parse_ty env) ty_l in
  let t_args = List.map (parse_term env) t_l in
  Term (term_apply env ast f ty_args t_args)

and parse_app_subst_ty env ast id args f_args body =
  let l = List.map (parse_ty env) args in
  Ty (ty_subst ast id l f_args body)

and parse_app_subst_term env ast id args f_ty_args f_t_args body =
  let n = List.length f_ty_args in
  let ty_l, t_l = take_drop n args in
  let ty_args = List.map (parse_ty env) ty_l in
  let t_args = List.map (parse_term env) t_l in
  Term (term_subst ast id ty_args t_args f_ty_args f_t_args body)

and parse_ty env ast =
  match parse_expr { env with expect = Type } ast with
  | Ty ty -> ty
  | _ -> _expected "type" ast

and parse_term env ast =
  match parse_expr { env with expect = Typed Msat.Expr.Ty.base } ast with
  | Term t -> t
  | _ -> _expected "term" ast

and parse_formula env ast =
  match parse_expr { env with expect = Typed Msat.Expr.Ty.prop } ast with
  | Term t when Msat.Expr.(Ty.equal Ty.prop t.t_type) ->
    make_pred ast t
  | Formula p -> p
  | _ -> _expected "formula" ast

let parse_ttype_var env t =
  match parse_var env t with
  | `Ty (id, v) -> (id, v)
  | `Term _ -> _expected "type variable" t

let rec parse_sig_quant env = function
  | { Ast.term = Ast.Binder (Ast.Pi, vars, t) } ->
    let ttype_vars = List.map (parse_ttype_var env) vars in
    let ttype_vars', env' = add_type_vars env ttype_vars in
    let l = List.combine vars ttype_vars' in
    parse_sig_arrow l [] env' t
  | t ->
    parse_sig_arrow [] [] env t

and parse_sig_arrow ttype_vars (ty_args: (Ast.t * res) list) env = function
  | { Ast.term = Ast.Binder (Ast.Arrow, args, ret) } ->
    let t_args = parse_sig_args env args in
    parse_sig_arrow ttype_vars (ty_args @ t_args) env ret
  | t ->
    begin match parse_expr env t with
      | Ttype ->
        begin match ttype_vars with
          | (h, _) :: _ ->
            raise (Typing_error (
                "Type constructor signatures cannot have quantified type variables", h))
          | [] ->
            let aux n = function
              | (_, Ttype) -> n + 1
              | (ast, _) -> raise (Found ast)
            in
            begin
              match List.fold_left aux 0 ty_args with
              | n -> `Ty_cstr n
              | exception Found err ->
                raise (Typing_error (
                    Format.asprintf
                      "Type constructor signatures cannot have non-ttype arguments,", err))
            end
        end
      | Ty ret ->
        let aux acc = function
          | (_, Ty t) -> t :: acc
          | (ast, _) -> raise (Found ast)
        in
        begin
          match List.fold_left aux [] ty_args with
          | exception Found err -> _expected "type" err
          | l -> `Fun_ty (List.map snd ttype_vars, List.rev l, ret)
        end
      | _ -> _expected "Ttype of type" t
    end

and parse_sig_args env l =
  flat_map (parse_sig_arg env) l

and parse_sig_arg env = function
  | { Ast.term = Ast.App ({ Ast.term = Ast.Builtin Ast.Product}, l) } ->
    List.map (fun x -> x, parse_expr env x) l
  | t ->
    [t, parse_expr env t]

let parse_sig = parse_sig_quant

let rec parse_fun ty_args t_args env = function
  | { Ast.term = Ast.Binder (Ast.Fun, l, ret) } ->
    let ty_args', t_args', env' = parse_quant_vars env l in
    parse_fun (ty_args @ ty_args') (t_args @ t_args') env' ret
  | ast ->
    begin match parse_expr env ast with
      | Ttype -> raise (Typing_error ("Cannot redefine Ttype", ast))
      | Ty body ->
        if t_args = [] then `Ty (ty_args, body)
        else _expected "term" ast
      | Term body -> `Term (ty_args, t_args, body)
      | Formula _ -> _expected "type or term" ast
    end

(* High-level parsing functions *)
(* ************************************************************************ *)

let new_decl id t =
  let env = empty_env () in
  Msat.Log.debugf 5 "Typing declaration: %s : %a"
    (fun k -> k id.Id.name Ast.print t);
  begin match parse_sig env t with
    | `Ty_cstr n -> decl_ty_cstr id (Msat.Expr.Id.ty_fun id.Id.name n)
    | `Fun_ty (vars, args, ret) ->
      decl_term id (Msat.Expr.Id.term_fun id.Id.name vars args ret)
  end

let new_def id t =
  let env = empty_env () in
  Msat.Log.debugf 5 "Typing definition: %s = %a"
    (fun k -> k id.Id.name Ast.print t);
  begin match parse_fun [] [] env t with
    | `Ty (ty_args, body) -> def_ty id ty_args body
    | `Term (ty_args, t_args, body) -> def_term id ty_args t_args body
  end

let new_formula t =
  let env = empty_env () in
  Msat.Log.debugf 5 "Typing top-level formula: %a" (fun k -> k Ast.print t);
  let res = parse_formula env t in
  res

