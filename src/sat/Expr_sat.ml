
exception Bad_atom
(** Exception raised if an atom cannot be created *)

type proof
(** A empty type for proofs *)

type t = int
(** Atoms are represented as integers. [-i] begin the negation of [i].
    Additionally, since we nee dot be able to create fresh atoms, we
    use even integers for user-created atoms, and odd integers for the
    fresh atoms. *)

let max_lit = max_int

(* Counters *)
let max_index = ref 0
let max_fresh = ref (-1)

(** Internal function for creating atoms.
    Updates the internal counters *)
let _make i =
  if i <> 0 && (abs i) < max_lit then begin
    max_index := max !max_index (abs i);
    i
  end else
    raise Bad_atom

(** A dummy atom *)
let dummy = 0

(** *)
let neg a = - a

let norm a =
  abs a, if a < 0 then
    Formula_intf.Negated
  else
    Formula_intf.Same_sign

let abs = abs

let sign i = i > 0

let apply_sign b i = if b then i else neg i

let set_sign b i = if b then abs i else neg (abs i)

let hash (a:int) = a land max_int
let equal (a:int) b = a=b
let compare (a:int) b = Pervasives.compare a b

let make i = _make (2 * i)

let fresh () =
  incr max_fresh;
  _make (2 * !max_fresh + 1)

(*
let iter: (t -> unit) -> unit = fun f ->
  for j = 1 to !max_index do
    f j
  done
*)

let print fmt a =
  Format.fprintf fmt "%s%s%d"
    (if a < 0 then "~" else "")
    (if a mod 2 = 0 then "v" else "f")
    ((abs a) / 2)
