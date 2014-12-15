

type ('a, 'b) t =
    | Left of 'a
    | Right of 'b

val mk_left : 'a -> ('a, 'b) t
val mk_right : 'b -> ('a, 'b) t

val destruct : ('a, 'b) t ->
    ('a -> 'c) -> ('b -> 'c) -> 'c
