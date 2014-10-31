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

; deriv求函数g的导数
(define dx 0.00001)
(define (deriv g)
    (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

; 逼近序列 x_n+1 = x_n - g(x_n) / Dg(x_n)
; 求逼近序列的不动点即为g(x)=0的解
(define (newton-transform g)
    (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newton-method g guess)
    (fixed-point (newton-transform g) guess))

; cubic函数 x^3+ax^2+bx+c
(define (cubic a b c)
    (lambda (x) (+ (* x x x) 
                (* a (square x))
                (* b x)
                c)))

; 牛顿法求x^3+x^2+2x+3=0的根
(newton-method (cubic 1 2 3) 1.0)
