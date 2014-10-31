(define (product term a next b) ; 迭代计算
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (* result (term a)))))
    (iter a 1))

; 利用product计算阶乘
(define (identity x) x)
(define (inc x) (+ x 1))
(define (factorial n)
    (product identity 1 inc n))
    
; 利用公式pi/4 = (2*4 * 4*6 ...) / (3*3 * 5*5 ...)计算pi
(define (pi-term x) (/ (* (- x 1) (+ x 1)) (square x)))
(define (pi-next x) (+ x 2))
(define (pi-prod n)
    (product pi-term 3 pi-next n))

(* 4.0 (pi-prod 1001))
