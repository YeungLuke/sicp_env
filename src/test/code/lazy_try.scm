(define (try a (b lazy-memo))
    (if (= a 0) 1 b))

(try 0 (/ 1 0))