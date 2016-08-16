(**************************************************************************)
(*                                                                        *)
(*                                  Cubicle                               *)
(*             Combining model checking algorithms and SMT solvers        *)
(*                                                                        *)
(*                  Sylvain Conchon and Alain Mebsout                     *)
(*                  Universite Paris-Sud 11                               *)
(*                                                                        *)
(*  Copyright 2011. This file is distributed under the terms of the       *)
(*  Apache Software License version 2.0                                   *)
(*                                                                        *)
(**************************************************************************)

open Msat

type 'a view =
  | Eq of 'a * 'a
  | Distinct of bool * 'a list
  | Builtin of bool * ID.t * 'a list

module type OrderedType = sig
  type t
  val compare : t -> t -> int
  val hash :  t -> int
  val print : Format.formatter -> t -> unit
end

module type S = sig
  type elt
  type t

  val make : elt view -> t
  val view : t -> elt view

  val neg : t -> t

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int

  module Map : Map.S with type key = t
  module Set : Set.S with type elt = t

end

module Make (X : OrderedType) : S with type elt = X.t = struct

  type elt = X.t
  type t = {
    view: X.t view;
    mutable tag: int;
  }

  module V = struct
    type t_ = t
    type t = t_

    let equal a1 a2 =
      match a1.view, a2.view with
      | Eq(t1, t2), Eq(u1, u2) ->
        (X.compare t1 u1 = 0 && X.compare t2 u2 = 0) ||
        (X.compare t1 u2 = 0 && X.compare t2 u1 = 0)
      | Distinct (b1,lt1), Distinct (b2,lt2) ->
        (try
           b1 = b2 &&
           List.for_all2 (fun x y -> X.compare x y = 0) lt1 lt2
         with Invalid_argument _ -> false)
      | Builtin(b1, n1, l1), Builtin(b2, n2, l2) ->
        (try
           b1 = b2 && ID.equal n1 n2
           &&
           List.for_all2 (fun x y -> X.compare x y = 0) l1 l2
         with Invalid_argument _ -> false)
      | _ -> false

    let hash a = match a.view with
      | Eq(t1, t2) -> abs (19 * (X.hash t1 + X.hash t2))
      | Distinct (b,lt) ->
        let x = if b then 7 else 23 in
        abs (17 * List.fold_left (fun acc t -> (X.hash t) + acc ) x lt)
      | Builtin(b, n, l) ->
        let x = if b then 7 else 23 in
        abs
          (List.fold_left
             (fun acc t-> acc*13 + X.hash t) (ID.hash n+x) l)

    let set_id t tag = t.tag <- tag
  end

  module H = Hashcons.Make(V)

  let compare a1 a2 = Pervasives.compare a1.tag a2.tag
  let equal a1 a2 = a1 == a2
  let hash a1 = a1.tag

  module T = struct
    type t_ = t
    type t = t_
    let compare=compare
  end

  let make t = H.hashcons {view=t; tag= -1}
  let view a = a.view

  let neg a = match view a with
    | Eq(x, y) -> make (Distinct (false,[x; y]))
    | Distinct (false, [x; y]) -> make (Eq (x, y))
    | Distinct (true, [x; y]) -> make (Distinct (false,[x; y]))
    | Distinct (false, l) -> make (Distinct (true,l))
    | Distinct _ -> assert false
    | Builtin(b, n, l) -> make (Builtin (not b, n, l))

  let print_list fmt = function
    | [] -> ()
    | z :: l ->
      Format.fprintf fmt "%a" X.print z;
      List.iter (Format.fprintf fmt ", %a" X.print) l

  let print fmt a = match view a with
    | Eq (z1, z2) ->
      if equal z1 z2 then Format.fprintf fmt "True"
      else Format.fprintf fmt "%a=%a" X.print z1 X.print z2
    | Distinct (b,(z::l)) ->
      let b = if b then "~" else "" in
      Format.fprintf fmt "%s%a" b X.print z;
      List.iter (fun x -> Format.fprintf fmt "<>%a" X.print x) l
    | Builtin (b, n, l) ->
      let b = if b then "" else "~" in
      Format.fprintf fmt "%s%s(%a)" b (ID.to_string n) print_list l
    | _ -> assert false

  module Set = Set.Make(T)
  module Map = Map.Make(T)

end

module type S_Term = sig

  include S with type elt = Term.t

  val mk_pred : Term.t -> t

  val true_ : t
  val false_ : t

  (*  val terms_of : t -> Term.Set.t
      val vars_of : t -> Symbols.Set.t
  *)
  (*  module SetEq : Set.S with type elt = t * Term.t * Term.t*)
end

module LT : S_Term = struct

  module L = Make(Term)
  include L

  let mk_pred t = make (Eq (t, Term.true_) )

  let true_ = mk_pred Term.true_
  let false_ = mk_pred Term.false_

  let neg a = match view a with
    | Eq(t1, t2) when Term.equal t2 Term.false_ ->
      make (Eq (t1, Term.true_))
    | Eq(t1, t2) when Term.equal t2 Term.true_ ->
      make (Eq (t1, Term.false_))
    | _ -> L.neg a

  (* let terms_of a =
     let l = match view a with
       | Eq (t1, t2) -> [t1; t2]
       | Distinct (_, l) | Builtin (_, _, l) -> l
     in
     List.fold_left Term.subterms Term.Set.empty l
  *)

  module SS = Symbols.Set
  (* let vars_of a =
     Term.Set.fold (fun t -> SS.union (Term.vars_of t)) (terms_of a) SS.empty
  *)
end

