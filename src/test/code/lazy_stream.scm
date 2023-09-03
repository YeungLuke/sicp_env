(define (cons (x lazy-memo) (y lazy-memo))
    (lambda ((m lazy-memo)) (m x y)))
(define (car (z lazy-memo))
    (z (lambda ((p lazy-memo) (q lazy-memo)) p)))
(define (cdr (z lazy-memo))
    (z (lambda ((p lazy-memo) (q lazy-memo)) q)))

(define (list-ref items n)
    (if (= n 0)
        (car items)
        (list-ref (cdr items) (- n 1))))

(define (map proc items)
    (if (null? items)
        nil
        (cons (proc (car items))
                (map proc (cdr items)))))

(define (scale-list items factor)
    (map (lambda (x) (* x factor))
            items))

(define (add-lists list1 list2)
    (cond ((null? list1) list2)
            ((null? list2) list1)
            (else (cons (+ (car list1) (car list2))
                        (add-lists (cdr list1) (cdr list2))))))

(define ones (cons 1 ones))
(define integers (cons 1 (add-lists ones integers)))

(define (integral (integrand lazy-memo) initial-value dt)
    (define int
        (cons initial-value
            (add-lists (scale-list integrand dt)
                        int)))
    int)
(define (solve f y0 dt)
    (define y (integral dy y0 dt))
    (define dy (map f y))
    y)
