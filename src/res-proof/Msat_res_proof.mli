
module type S = Msat_res_proof_intf.S

module Make(A : Msat.PROOF_ARG)
  : S with module Formula = A.Formula
       and module Clause = A.Clause
       and module Atom = A.Atom

  include Msat.PROOF_BUILDER
with T
