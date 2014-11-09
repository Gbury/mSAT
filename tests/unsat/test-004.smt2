(declare-fun a () Bool)
(declare-fun b () Bool)
(assert (and (or a b) (not a) (not b)))
(check-sat)
