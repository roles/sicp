(define (cont-frac n d k) ; 迭代计算过程
    (define (frac i prev)
        (/ (n i) (- (d i) prev)))
    (define (iter i result)
        (if (= i 0)
            result
            (iter (- i 1) (frac i result))))
    (iter k 0))

; 利用lambert公式逼近正切值tan
(define (tan-cf x k)
    (define (d i) (- (* 2 i) 1))
    (define (n i) 
        (if (= i 1)
            x
            (square x)))
    (cont-frac n d k))
