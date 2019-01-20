(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2016 Guillaume Bury
*)

module Int_lit = Int_lit

include Msat.Make_pure_sat(struct
    module Formula = Int_lit
    module Proof = Msat.Proof_empty
  end)

