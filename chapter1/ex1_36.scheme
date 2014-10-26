(define tolerance 0.0001)

; 函数x -> f(x)不动点的逼近方式
; f(x), f(f(x)), f(f(f(x)))...
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

; 利用不动点计算x^x=1000的根
(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)

; 使用平均阻尼,加速收敛
(fixed-point (lambda (x) (/ (+ (/ (log 1000) (log x)) x) 2.0)) 2.0)
