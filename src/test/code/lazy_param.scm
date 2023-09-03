(define (f a (b lazy) (c lazy-memo))
    (+ a c))

(f 1 (/ 1 0) 3)