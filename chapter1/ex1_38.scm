(define (cont-frac n d k) ; 迭代计算过程
    (define (frac i prev)
        (/ (n i) (+ (d i) prev)))
    (define (iter i result)
        (if (= i 0)
            result
            (iter (- i 1) (frac i result))))
    (iter k 0))

(define (d i) 
    (define (divide-three? n) (= (remainder n 3) 0))
    (let ((next (+ i 1)))
        (cond ((divide-three? next) (* 2 (/ next 3)))
              (else 1.0))))

; 无限连分式中, Ni为1, Di为1 2 1 1 4 1 1 6 ....
; 可以逼近e - 2
(+ 2 (cont-frac (lambda (x) 1.0)
                d
                100))
