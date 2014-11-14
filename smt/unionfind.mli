module type OrderedType = sig
    type t
    val compare : t -> t -> int
end

module Make(T : OrderedType) : sig
    type t
    exception Unsat
    val empty : t
    val find : t -> T.t -> T.t
    val union : t -> T.t -> T.t -> t
    val forbid : t -> T.t -> T.t -> t
end

