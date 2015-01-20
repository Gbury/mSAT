
module type S = sig
    val debug : int -> ('a, Buffer.t, unit, unit) format4 -> 'a
    (** debug message *)
end

