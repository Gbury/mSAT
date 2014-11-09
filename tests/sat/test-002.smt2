(declare-fun a () Bool)
(declare-fun b () Bool)
(assert (or a b))
(check-sat)
