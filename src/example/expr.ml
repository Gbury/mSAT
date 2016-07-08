(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

module I = Formula_intf

exception Invalid_var

type var = string
type formula =
  | Prop of int
  | Equal of var * var
  | Distinct of var * var
type t = formula
type proof = unit

let dummy = Prop 0

let max_fresh = ref 0
let fresh () =
  incr max_fresh;
  Prop (2 * !max_fresh + 1)

let mk_prop i =
  if i <> 0 && i < max_int / 2 then Prop (2 * i)
  else raise Invalid_var

let mk_var i =
  if i <> "" then i
  else raise Invalid_var

let mk_eq i j = Equal (mk_var (min i j), mk_var (max i j))
let mk_neq i j = Distinct (mk_var (min i j), mk_var (max i j))

let neg = function
  | Prop i -> Prop (-i)
  | Equal (a, b) -> Distinct (a, b)
  | Distinct (a, b) -> Equal (a, b)

let norm = function
  | Prop i -> Prop (abs i), if i < 0 then I.Negated else I.Same_sign
  | Equal (a, b) -> Equal (a, b), I.Same_sign
  | Distinct (a, b) -> Equal (a, b), I.Negated

(* Only used after normalisation, so usual functions should work *)
let hash = Hashtbl.hash
let equal = (=)
let compare = Pervasives.compare

let print fmt = function
  | Prop i -> Format.fprintf fmt "%s%s%d" (if i < 0 then "¬ " else "") (if i mod 2 = 0 then "v" else "f") ((abs i) / 2)
  | Equal (a, b) -> Format.fprintf fmt "%s = %s" a b
  | Distinct (a, b) -> Format.fprintf fmt "%s ≠ %s" a b

module Term = struct
  type t = var
  let hash = Hashtbl.hash
  let equal = (=)
  let compare = Pervasives.compare
  let print fmt t = Format.fprintf fmt "%s" t
end

module Formula = struct
  type t = formula
  let hash = Hashtbl.hash
  let equal = (=)
  let compare = Pervasives.compare
  let print = print
end

