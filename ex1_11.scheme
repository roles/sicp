(define (f-recur n)
    (cond ((< n 3) n)
          (else (+ (f-recur (- n 1)) (* 2 (f-recur (- n 2))) (* 3 (f-recur (- n 3)))))))

(define (f-iter n)
    (define (f-sub-iter i a b c)
        (cond ((< n 3) n)
              ((= i n) a)
              (else (f-sub-iter (+ i 1) 
                            (+ a (* 2 b) (* 3 c))
                            a
                            b))))
    (f-sub-iter 2 2 1 0))

