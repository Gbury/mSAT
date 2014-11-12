(declare-fun a () Bool)
(declare-fun b () Bool)
(assert (and (or a b) false (not a) (not b)))
(check-sat)
