module type ELEMENTS = Heap_intf.ELEMENTS

module type S = Heap_intf.S

module Make(E : ELEMENTS)
  : S with type elt = E.elt
       and type t = E.t
