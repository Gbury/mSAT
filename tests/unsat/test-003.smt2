(declare-fun a () Bool)
(declare-fun b () Bool)
(assert (and (or a b) (and (not a) (not b))))
(check-sat)
