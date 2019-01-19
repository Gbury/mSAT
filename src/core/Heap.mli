module type RANKED = Heap_intf.RANKED

module type S = Heap_intf.S

module Make(X : RANKED) : S with type elt = X.t
