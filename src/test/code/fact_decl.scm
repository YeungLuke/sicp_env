(define fact-decl
    (lambda (n)
        (define iter
            (lambda (m i)
                (if (> i n)
                    m
                    (iter (* m i)
                          (+ i 1)))))
        (iter 1 1)))
(fact-decl 5)