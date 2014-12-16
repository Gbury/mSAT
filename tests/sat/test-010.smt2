(assert (and (= a b) (= b c) (or (not (= a c)) (= a d))))
(check-sat)
