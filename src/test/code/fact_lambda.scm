((lambda (n)
  ((lambda (f)
    (f f n))
    (lambda (ft k)
        (if (= k 1)
            1
            (* k (ft ft (- k 1)))))))
 5)