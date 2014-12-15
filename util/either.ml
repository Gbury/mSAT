
type ('a, 'b) t =
    | Left of 'a
    | Right of 'b

let mk_left a = Left a
let mk_right b = Right b

let destruct e f_left f_right = match e with
    | Left a -> f_left a
    | Right b -> f_right b
