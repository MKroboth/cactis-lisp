(def 'fac (lambda (x) (cond ((= 0 x) 1) ('else (* x (fac (- x 1)))))))
