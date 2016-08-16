(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)
open Msat

module I = Formula_intf
module Term = Term

exception Invalid_prop

type term = Term.t

(* atomic formula

   Prop:
   - sign of int determines sign of formula
   - odd numbers --> fresh atoms (for Tseitin CNF)
   - even numbers --> used for regular int-mapping
*)
type formula =
  | Prop of int
  | Equal of term * term
  | Distinct of term * term

type t = formula
type proof = unit

let dummy = Prop 0

let max_fresh = ref 0

let fresh () =
  incr max_fresh;
  Prop (2 * !max_fresh + 1)

let mk_prop i =
  if i <> 0 && i < max_int / 2 then Prop (2 * i)
  else raise Invalid_prop

let order_ t1 t2 = if Term.compare t1 t2 > 0 then t2,t1 else t1,t2

let mk_eq a b =
  let a, b = order_ a b in
  Equal (a, b)

let mk_neq a b =
  let a, b = order_ a b in
  Distinct (a, b)

let mk_true = mk_eq Term.true_ Term.true_
let mk_false = mk_eq Term.true_ Term.false_
let mk_atom = mk_eq Term.true_
let mk_atom_neg = mk_eq Term.false_

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
  | Prop i ->
    Format.fprintf fmt "%s%s%d"
      (if i < 0 then "¬ " else "")
      (if i mod 2 = 0 then "v" else "f") ((abs i) / 2)
  | Equal (a, b) -> Format.fprintf fmt "(@[=@ %a@ %a@])" Term.print a Term.print b
  | Distinct (a, b) -> Format.fprintf fmt "(@[!=@ %a@ %a@])" Term.print a Term.print b

module Formula = struct
  type t = formula
  let hash = Hashtbl.hash
  let equal = (=)
  let compare = Pervasives.compare
  let print = print
end

