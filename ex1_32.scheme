; 定义一般性的累积函数
(define (accumulate combiner null-value term a next b) ; 迭代计算过程
    (define (iter a result) 
        (if (> a b)
            result
            (iter (next a) (combiner (term a) result))))
    (iter a null-value))

(define (add a b) (+ a b))
(define (sum term a next b)
    (accumulate add 0 term a next b))

(define (mul a b) (* a b))
(define (product term a next b)
    (accumulate mul 1 term a next b))
