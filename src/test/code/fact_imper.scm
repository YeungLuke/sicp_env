(define fact-imper
    (lambda (n)
        (define m 1)
        (define i 1)
        (define loop
            (lambda ()
                (if (> i n)
                    m
                    (begin (set! m (* m i))
                           (set! i (+ i 1))
                           (loop)))))
        (loop)))
(fact-imper 5)