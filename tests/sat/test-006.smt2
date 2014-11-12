(declare-fun a () Bool)
(declare-fun b () Bool)
(assert (or (and a b) true (not a) (not b)))
(check-sat)
