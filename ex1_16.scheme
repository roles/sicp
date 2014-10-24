; 求幂的迭代计算过程
; 维护三个状态量a,b,n，a*b^n为不变量
; n为奇数时，a*b^n = (a*b)*b^(n-1)
; n为偶数时，a*b^n = a*(b*b)^(n/2)
(define (fast-expt b n)
    (define (even? x) (= (remainder x 2) 0))
    (define (fast-expt-iter a b n)
        (cond ((= n 0) a)
              ((even? n) (fast-expt-iter a (square b) (/ n 2)))
              (else (fast-expt-iter (* a b) b (- n 1)))))
    (fast-expt-iter 1 b n))
