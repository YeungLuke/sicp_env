(define (square x) (* x x))

(define (abs x)
    (if (< x 0)
        (- x)
        x))

(define (sqrt-iter guess x)
    ;(display guess)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x)
                   x)))

(define (improve guess x)
    (average guess (/ x guess)))

(define (average x y)
    (/ (+ x y) 2))

(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
    (sqrt-iter 1.0 x))

(display "(sqrt 9): ")
(display (sqrt 9))
(newline)
(display "(sqrt (+ (sqrt 2) (sqrt 3))): ")
(display (sqrt (+ (sqrt 2) (sqrt 3))))
(newline)