(**************************************************************************)
(*                                                                        *)
(*                                  Cubicle                               *)
(*             Combining model checking algorithms and SMT solvers        *)
(*                                                                        *)
(*                  Sylvain Conchon, Evelyne Contejean                    *)
(*                  Francois Bobot, Mohamed Iguernelala, Alain Mebsout    *)
(*                  CNRS, Universite Paris-Sud 11                         *)
(*                                                                        *)
(*  Copyright 2011. This file is distributed under the terms of the       *)
(*  Apache Software License version 2.0                                   *)
(*                                                                        *)
(**************************************************************************)

module T  : sig type t = Term.t end
module S  : sig type t = Symbols.t end
module ST : sig type elt = T.t type t = Term.Set.t end
module SA : Set.S with type elt = Literal.LT.t * Explanation.t
  
type elt = ST.t * SA.t
    
module Make :
  functor (X : Sig.X) ->
sig
  
  type t 
  val empty : t
  val find : X.r -> t -> elt
  val add : X.r -> elt -> t -> t
  val mem : X.r -> t -> bool
  val print : t -> unit
  val up_add : t -> ST.elt -> X.r -> X.r list -> t
      
  val congr_add : t -> X.r list -> ST.t
  
  val up_close_up :t -> X.r -> X.r -> t
  val congr_close_up : t -> X.r -> X.r list -> elt
end
