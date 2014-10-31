(define (fib n)
    (define (even? x) (= (remainder x 2) 0))
    (define (fib-iter a b p q n)
        (cond ((= 0 n) b)
              ((even? n) (fib-iter a 
                                   b
                                   (+ (square p) (square q))
                                   (+ (square q) (* 2 p q))
                                   (/ n 2)))
              (else (fib-iter (+ (* b q) (* a q) (* a p))
                              (+ (* b p) (* a q))
                              p
                              q
                              (- n 1)))))
    (fib-iter 1 0 0 1 n))
