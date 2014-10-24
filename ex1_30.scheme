(define (cube x) (* x x x))
(define (even? x) (= (remainder x 2) 0))

(define (sum term a next b) ; 迭代计算
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (+ result (term a)))))
    (iter a 0))

(define (integral f a b dx)
    (define (add-dx x) (+ x dx))
    (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

; 辛普森规则计算函数f在范围a和b之间的定积分近似值
; (h/3) * [y_0+4y_1+2y_2+...+4y_n-1+y_n]
; h=(b-a)/n, y_k=f(a+kh), n是某个偶数
(define (simpson-integral f a b n)
    (define h (/ (- b a) n))
    (define (add-h x) (+ x h))
    (define (f-iter a k) ; 带迭代计数
        (cond ((or (= k n) (= k 0)) (f a))
              ((even? k) (* (f a) 2))
              (else (* (f a) 4))))
    (define (iter a k result) ; 迭代计算
        (if (> k n)
            result
            (iter (add-h a) (+ k 1) (+ result (f-iter a k)))))
    (* (iter a 0 0) (/ h 3.0)))
