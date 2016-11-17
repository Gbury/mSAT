
module type ARG = sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
  val set_id : t -> int -> unit
end

module Make(A : ARG): sig
  val hashcons : A.t -> A.t
  val iter : (A.t -> unit) -> unit
end = struct
  module W = Weak.Make(A)

  let tbl_ = W.create 1024
  let n_ = ref 0

  (* hashcons terms *)
  let hashcons t =
    let t' = W.merge tbl_ t in
    if t == t' then (
      incr n_;
      A.set_id t' !n_;
    );
    t'

  let iter yield = W.iter yield tbl_
end
