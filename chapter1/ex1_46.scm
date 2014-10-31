; 迭代式改进策略
(define (iterative-improve good-enough? improve)
    (define (iter guess)
        (if (good-enough? guess)
            guess
            (iter (improve guess))))
    iter)

(define (sqrt-iter n)
    (define (sqrt-improve guess) (/ (+ guess (/ n guess)) 2.0))
    (define (sqrt-good-enough? guess)
        (< (/ (abs (- guess (sqrt-improve guess))) guess) 0.001))
    ((iterative-improve sqrt-good-enough? sqrt-improve) 1.0))

; (sqrt-iter 2)

(define (fixed-point-iter f first-guess)
    (define (improve guess) (f guess))
    (define (good-enough? guess)
        (< (/ (abs (- guess (improve guess))) guess) 0.001))
    ((iterative-improve good-enough? improve) first-guess))

(fixed-point-iter (lambda (x) (+ 1 (/ 1.0 x))) 1.0)
