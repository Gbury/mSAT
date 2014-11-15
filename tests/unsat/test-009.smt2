(assert (and (= a b) (or (= b c) (= b d)) (not (= a d)) (not (= a c))))
(check-sat)
