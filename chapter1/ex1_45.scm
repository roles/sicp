; fixed-point计算不动点
(define tolerance 0.0001)
(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))
    (define (try guess k)
        (let ((next (f guess)))
            (newline) ; 统计计算步数
            (display k)
            (display "\t")
            (display next)
            (if (close-enough? guess next)
                next
                (try next (+ k 1)))))
    (try first-guess 1))

; average-damp对函数进行一次平均阻尼转换
(define (average a b) (/ (+ a b) 2.0))
(define (average-damp f)
    (lambda (x) (average x (f x))))

; repeated将函数施加n次
(define (compose f g)
    (lambda (x) (f (g x))))
(define (repeated f n)
    (if (= n 1)
        f
        (compose f (repeated f (- n 1)))))

; 迭代计算求幂
(define (fast-expt b n)
    (define (even? x) (= (remainder x 2) 0))
    (define (fast-expt-iter a b n)
        (cond ((= n 0) a)
              ((even? n) (fast-expt-iter a (square b) (/ n 2)))
              (else (fast-expt-iter (* a b) b (- n 1)))))
    (fast-expt-iter 1 b n))

; 求n次方根,进行k次平均阻尼
; 等价求 y -> x/y^(n-1) 的不动点
(define (root-n-test x n k)
    (fixed-point ((repeated average-damp k) (lambda (y) (/ x (fast-expt y (- n 1))))) 1.0))

; 求n次方根,进行n-1次平均阻尼（最佳）
(define (root-n x n)
    (fixed-point ((repeated average-damp (- n 1)) (lambda (y) (/ x (fast-expt y (- n 1))))) 1.0))

