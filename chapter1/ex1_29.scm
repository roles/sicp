(define (cube x) (* x x x))
(define (even? x) (= (remainder x 2) 0))

(define (sum term a next b) ; 线性递归
    (if (> a b)
        0
        (+ (term a)
           (sum term (next a) next b))))

(define (integral f a b dx)
    (define (add-dx x) (+ x dx))
    (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

; 辛普森规则计算函数f在范围a和b之间的定积分近似值
; (h/3) * [y_0+4y_1+2y_2+...+4y_n-1+y_n]
; h=(b-a)/n, y_k=f(a+kh), n是某个偶数
(define (simpson-integral f a b n)
    (define h (/ (- b a) n))
    (define (add-h x) (+ x h))
    (define (simpson-add term a next k n)
        (cond ((> k n) 0)
              (else (+ (cond ((or (= k n) (= k 0)) (term a))
                             ((even? k) (* (term a) 2))
                             (else (* (term a) 4)))
                       (simpson-add term (next a) next (+ k 1) n)))))
    (* (simpson-add f a add-h 0 n) (/ h 3.0)))
