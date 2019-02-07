
type 'a t = {
  mutable cur: 'a;
  stack: 'a Vec.t;
  copy: ('a -> 'a) option;
}

let create ?copy x: _ t =
  {cur=x; stack=Vec.create(); copy}

let[@inline] get self = self.cur
let[@inline] set self x = self.cur <- x
let[@inline] update self f = self.cur <- f self.cur

let[@inline] n_levels self = Vec.size self.stack

let[@inline] push_level self : unit =
  let x = self.cur in
  let x = match self.copy with None -> x | Some f -> f x in
  Vec.push self.stack x

let pop_levels self n : unit =
  assert (n>=0);
  if n > Vec.size self.stack then invalid_arg "Backtrackable_ref.pop_levels";
  let i = Vec.size self.stack-n in
  let x = Vec.get self.stack i in
  self.cur <- x;
  Vec.shrink self.stack i;
  ()
