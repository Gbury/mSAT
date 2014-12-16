(assert (and (= a b) (= b c) (= c d) (or (not (= a c)) (not (= a a)))))
(check-sat)
