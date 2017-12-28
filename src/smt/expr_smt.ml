(*
   Base modules that defines the terms used in the prover.
*)

(* Type definitions *)
(* ************************************************************************ *)

(* Private aliases *)
type hash = int
type index = int

(* Identifiers, parametrized by the kind of the type of the variable *)
type 'ty id = {
  id_type : 'ty;
  id_name : string;
  index   : index; (** unique *)
}

(* Type for first order types *)
type ttype = Type

(* The type of functions *)
type 'ty function_descr = {
  fun_vars : ttype id list; (* prenex forall *)
  fun_args : 'ty list;
  fun_ret : 'ty;
}

(* Types *)
type ty_descr =
  | TyVar of ttype id (** Bound variables *)
  | TyApp of ttype function_descr id * ty list

and ty = {
  ty : ty_descr;
  mutable ty_hash : hash; (** lazy hash *)
}

(* Terms & formulas *)
type term_descr =
  | Var of ty id
  | App of ty function_descr id * ty list * term list

and term = {
  term    : term_descr;
  t_type  : ty;
  mutable t_hash : hash; (* lazy hash *)
}

type atom_descr =
  | Pred of term
  | Equal of term * term

and atom = {
  sign : bool;
  atom : atom_descr;
  mutable f_hash  : hash; (* lazy hash *)
}

(* Utilities *)
(* ************************************************************************ *)

let rec list_cmp ord l1 l2 =
  match l1, l2 with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | x1::l1', x2::l2' ->
    let c = ord x1 x2 in
    if c = 0
    then list_cmp ord l1' l2'
    else c

(* Exceptions *)
(* ************************************************************************ *)

exception Type_mismatch of term * ty * ty
exception Bad_arity of ty function_descr id * ty list * term list
exception Bad_ty_arity of ttype function_descr id * ty list

(* Printing functions *)
(* ************************************************************************ *)

module Print = struct
  let rec list f sep fmt = function
    | [] -> ()
    | [x] -> f fmt x
    | x :: ((_ :: _) as r) ->
      Format.fprintf fmt "%a%s" f x sep;
      list f sep fmt r

  let id fmt v = Format.fprintf fmt "%s" v.id_name
  let ttype fmt = function Type -> Format.fprintf fmt "Type"

  let rec ty fmt t = match t.ty with
    | TyVar v -> id fmt v
    | TyApp (f, []) ->
      Format.fprintf fmt "%a" id f
    | TyApp (f, l) ->
      Format.fprintf fmt "%a(%a)" id f (list ty ", ") l

  let params fmt = function
    | [] -> ()
    | l -> Format.fprintf fmt "∀ %a. " (list id ", ") l

  let signature print fmt f =
    match f.fun_args with
    | [] -> Format.fprintf fmt "%a%a" params f.fun_vars print f.fun_ret
    | l -> Format.fprintf fmt "%a%a -> %a" params f.fun_vars
             (list print " -> ") l print f.fun_ret

  let fun_ty = signature ty
  let fun_ttype = signature ttype

  let id_type print fmt v = Format.fprintf fmt "%a : %a" id v print v.id_type

  let id_ty = id_type ty
  let id_ttype = id_type ttype
  let const_ty = id_type fun_ty
  let const_ttype = id_type fun_ttype

  let rec term fmt t = match t.term with
    | Var v -> id fmt v
    | App (f, [], []) ->
      Format.fprintf fmt "%a" id f
    | App (f, [], args) ->
      Format.fprintf fmt "%a(%a)" id f
        (list term ", ") args
    | App (f, tys, args) ->
      Format.fprintf fmt "%a(%a; %a)" id f
        (list ty ", ") tys
        (list term ", ") args

  let atom_aux fmt f =
    match f.atom with
    | Equal (a, b) ->
      Format.fprintf fmt "%a %s %a"
        term a (if f.sign then "=" else "<>") term b
    | Pred t ->
      Format.fprintf fmt "%s%a" (if f.sign then "" else "¬ ") term t

  let atom fmt f = Format.fprintf fmt "⟦%a⟧" atom_aux f

end

(* Substitutions *)
(* ************************************************************************ *)

module Subst = struct
  module Mi = Map.Make(struct
      type t = int * int
      let compare (a, b) (c, d) = match compare a c with 0 -> compare b d | x -> x
    end)

  type ('a, 'b) t = ('a * 'b) Mi.t

  (* Usual functions *)
  let empty = Mi.empty

  let is_empty = Mi.is_empty

  let iter f = Mi.iter (fun _ (key, value) -> f key value)

  let fold f = Mi.fold (fun _ (key, value) acc -> f key value acc)

  let bindings s = Mi.fold (fun _ (key, value) acc -> (key, value) :: acc) s []

  (* Comparisons *)
  let equal f = Mi.equal (fun (_, value1) (_, value2) -> f value1 value2)
  let compare f = Mi.compare (fun (_, value1) (_, value2) -> f value1 value2)
  let hash h s = Mi.fold (fun i (_, value) acc -> Hashtbl.hash (acc, i, h value)) s 1

  let choose m = snd (Mi.choose m)

  (* Iterators *)
  let exists pred s =
    try
      iter (fun m s -> if pred m s then raise Exit) s;
      false
    with Exit ->
      true

  let for_all pred s =
    try
      iter (fun m s -> if not (pred m s) then raise Exit) s;
      true
    with Exit ->
      false

  let print print_key print_value fmt map =
    let aux _ (key, value) =
      Format.fprintf fmt "%a -> %a@ " print_key key print_value value
    in
    Format.fprintf fmt "@[<hov 0>%a@]" (fun _ -> Mi.iter aux) map

  module type S = sig
    type 'a key
    val get : 'a key -> ('a key, 'b) t -> 'b
    val mem : 'a key -> ('a key, 'b) t -> bool
    val bind : 'a key -> 'b -> ('a key, 'b) t -> ('a key, 'b) t
    val remove : 'a key -> ('a key, 'b) t -> ('a key, 'b) t
  end

  (* Variable substitutions *)
  module Id = struct
    type 'a key = 'a id
    let tok v = (v.index, 0)
    let get v s = snd (Mi.find (tok v) s)
    let mem v s = Mi.mem (tok v) s
    let bind v t s = Mi.add (tok v) (v, t) s
    let remove v s = Mi.remove (tok v) s
  end

end

(* Dummies *)
(* ************************************************************************ *)

module Dummy = struct

  let id_ttype =
    { index = -1; id_name = "<dummy>"; id_type = Type; }

  let ty =
    { ty = TyVar id_ttype; ty_hash = -1; }

  let id =
    { index = -2; id_name = "<dummy>"; id_type = ty; }

  let term =
    { term = Var id; t_type = ty; t_hash = -1; }

  let atom =
    { atom = Pred term; sign = true; f_hash = -1; }

end

(* Variables *)
(* ************************************************************************ *)

module Id = struct
  type 'a t = 'a id

  (* Hash & comparisons *)
  let hash v = v.index

  let compare: 'a. 'a id -> 'a id -> int =
    fun v1 v2 -> compare v1.index v2.index

  let equal v1 v2 = compare v1 v2 = 0

  (* Printing functions *)
  let print = Print.id

  (* Id count *)
  let _count = ref 0

  (* Constructors *)
  let mk_new id_name id_type =
    incr _count;
    let index = !_count in
    { index; id_name; id_type }

  let ttype name = mk_new name Type
  let ty name ty = mk_new name ty

  let const name fun_vars fun_args fun_ret =
    mk_new name { fun_vars; fun_args; fun_ret; }

  let ty_fun name n =
    let rec replicate acc n =
      if n <= 0 then acc
      else replicate (Type :: acc) (n - 1)
    in
    const name [] (replicate [] n) Type

  let term_fun = const

  (* Builtin Types *)
  let prop = ty_fun "Prop" 0
  let base = ty_fun "$i" 0

end

(* Types *)
(* ************************************************************************ *)

module Ty = struct
  type t = ty
  type subst = (ttype id, ty) Subst.t

  (* Hash & Comparisons *)
  let rec hash_aux t = match t.ty with
    | TyVar v -> Id.hash v
    | TyApp (f, args) ->
      Hashtbl.hash (Id.hash f, List.map hash args)

  and hash t =
    if t.ty_hash = -1 then
      t.ty_hash <- hash_aux t;
    t.ty_hash

  let discr ty = match ty.ty with
    | TyVar _ -> 1
    | TyApp _ -> 2

  let rec compare u v =
    let hu = hash u and hv = hash v in
    if hu <> hv then Pervasives.compare hu hv
    else match u.ty, v.ty with
      | TyVar v1, TyVar v2 -> Id.compare v1 v2
      | TyApp (f1, args1), TyApp (f2, args2) ->
        begin match Id.compare f1 f2 with
          | 0 -> list_cmp compare args1 args2
          | x -> x
        end
      | _, _ -> Pervasives.compare (discr u) (discr v)

  let equal u v =
    u == v || (hash u = hash v && compare u v = 0)

  (* Printing functions *)
  let print = Print.ty

  (* Constructors *)
  let mk_ty ty = { ty; ty_hash = -1; }

  let of_id v = mk_ty (TyVar v)

  let apply f args =
    assert (f.id_type.fun_vars = []);
    if List.length args <> List.length f.id_type.fun_args then
      raise (Bad_ty_arity (f, args))
    else
      mk_ty (TyApp (f, args))

  (* Builtin types *)
  let prop = apply Id.prop []
  let base = apply Id.base []

  (* Substitutions *)
  let rec subst_aux map t = match t.ty with
    | TyVar v -> begin try Subst.Id.get v map with Not_found -> t end
    | TyApp (f, args) ->
      let new_args = List.map (subst_aux map) args in
      if List.for_all2 (==) args new_args then t
      else apply f new_args

  let subst map t = if Subst.is_empty map then t else subst_aux map t

  (* Typechecking *)
  let instantiate f tys args =
    if List.length f.id_type.fun_vars <> List.length tys ||
       List.length f.id_type.fun_args <> List.length args then
      raise (Bad_arity (f, tys, args))
    else
      let map = List.fold_left2 (fun acc v ty -> Subst.Id.bind v ty acc) Subst.empty f.id_type.fun_vars tys in
      let fun_args = List.map (subst map) f.id_type.fun_args in
      List.iter2 (fun t ty ->
          if not (equal t.t_type ty) then raise (Type_mismatch (t, t.t_type, ty)))
        args fun_args;
      subst map f.id_type.fun_ret

end

(* Terms *)
(* ************************************************************************ *)

module Term = struct
  type t = term
  type subst = (ty id, term) Subst.t

  (* Hash & Comparisons *)
  let rec hash_aux t = match t.term with
    | Var v -> Id.hash v
    | App (f, tys, args) ->
      let l = List.map Ty.hash tys in
      let l' = List.map hash args in
      Hashtbl.hash (Id.hash f, l, l')

  and hash t =
    if t.t_hash = -1 then
      t.t_hash <- hash_aux t;
    t.t_hash

  let discr t = match t.term with
    | Var _ -> 1
    | App _ -> 2

  let rec compare u v =
    let hu = hash u and hv = hash v in
    if hu <> hv then Pervasives.compare hu hv
    else match u.term, v.term with
      | Var v1, Var v2 -> Id.compare v1 v2
      | App (f1, tys1, args1), App (f2, tys2, args2) ->
        begin match Id.compare f1 f2 with
          | 0 ->
            begin match list_cmp Ty.compare tys1 tys2 with
              | 0 -> list_cmp compare args1 args2
              | x -> x
            end
          | x -> x
        end
      | _, _ -> Pervasives.compare (discr u) (discr v)

  let equal u v =
    u == v || (hash u = hash v && compare u v = 0)

  (* Printing functions *)
  let print = Print.term

  (* Constructors *)
  let mk_term term t_type =
    { term; t_type; t_hash = -1; }

  let of_id v =
    mk_term (Var v) v.id_type

  let apply f ty_args t_args =
    mk_term (App (f, ty_args, t_args)) (Ty.instantiate f ty_args t_args)

  (* Substitutions *)
  let rec subst_aux ty_map t_map t = match t.term with
    | Var v -> begin try Subst.Id.get v t_map with Not_found -> t end
    | App (f, tys, args) ->
      let new_tys = List.map (Ty.subst ty_map) tys in
      let new_args = List.map (subst_aux ty_map t_map) args in
      if List.for_all2 (==) new_tys tys && List.for_all2 (==) new_args args then t
      else apply f new_tys new_args

  let subst ty_map t_map t =
    if Subst.is_empty ty_map && Subst.is_empty t_map then
      t
    else
      subst_aux ty_map t_map t

  let rec replace (t, t') t'' = match t''.term with
    | _ when equal t t'' -> t'
    | App (f, ty_args, t_args) ->
      apply f ty_args (List.map (replace (t, t')) t_args)
    | _ -> t''

end

(* Formulas *)
(* ************************************************************************ *)

module Atom = struct
  type t = atom

  type proof = unit

  (* Hash & Comparisons *)
  let h_eq    = 2
  let h_pred  = 3

  let rec hash_aux f = match f.atom with
    | Equal (t1, t2) ->
      Hashtbl.hash (h_eq, Term.hash t1, Term.hash t2)
    | Pred t ->
      Hashtbl.hash (h_pred, Term.hash t)

  and hash f =
    if f.f_hash = -1 then
      f.f_hash <- hash_aux f;
    f.f_hash

  let discr f = match f.atom with
    | Equal _ -> 1
    | Pred _ -> 2

  let compare f g =
    let hf = hash f and hg = hash g in
    if hf <> hg then Pervasives.compare hf hg
    else match f.atom, g.atom with
      | Equal (u1, v1), Equal(u2, v2) ->
        list_cmp Term.compare [u1; v1] [u2; v2]
      | Pred t1, Pred t2 -> Term.compare t1 t2
      | _, _ -> Pervasives.compare (discr f) (discr g)

  let equal u v =
    u == v || (hash u = hash v && compare u v = 0)

  (* Printing functions *)
  let print = Print.atom

  (* Constructors *)
  let mk_formula f = {
    sign = true;
    atom = f;
    f_hash = -1;
  }

  let dummy = Dummy.atom

  let pred t =
    if not (Ty.equal Ty.prop t.t_type) then
      raise (Type_mismatch (t, t.t_type, Ty.prop))
    else
      mk_formula (Pred t)

  let fresh () =
    let id = Id.ty "fresh" Ty.prop in
    pred (Term.of_id id)

  let neg f =
    { f with sign = not f.sign }

  let eq a b =
    if not (Ty.equal a.t_type b.t_type) then
      raise (Type_mismatch (b, b.t_type, a.t_type))
    else if Term.compare a b < 0 then
      mk_formula (Equal (a, b))
    else
      mk_formula (Equal (b, a))

  let norm f =
    { f with sign = true },
    if f.sign then Formula_intf.Same_sign
    else Formula_intf.Negated

end

module Formula = Msat_tseitin.Make(Atom)

