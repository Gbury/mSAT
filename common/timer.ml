(**************************************************************************)
(*                                                                        *)
(*                                  Cubicle                               *)
(*             Combining model checking algorithms and SMT solvers        *)
(*                                                                        *)
(*                  Sylvain Conchon and Alain Mebsout                     *)
(*                  Universite Paris-Sud 11                               *)
(*                                                                        *)
(*  Copyright 2011. This file is distributed under the terms of the       *)
(*  Apache Software License version 2.0                                   *)
(*                                                                        *)
(**************************************************************************)

module type S = sig
  val start : unit -> unit
  val pause : unit -> unit
  val get : unit -> float    
end

module Make (X : sig end) = struct

  open Unix
    
  let u = ref 0.0

  let cpt = ref 0.0
    
  let start () = u:=(times()).tms_utime

  let pause () = cpt := !cpt +. ((times()).tms_utime -. !u)

  let get () = 
    !cpt

end
