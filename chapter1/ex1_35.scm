(define tolerance 0.0001)

(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))
    (define (try guess)
        (let ((next (f guess)))
            (if (close-enough? guess next)
                next
                (try next))))
    (try first-guess))

; 寻找x->1+1/x的不动点
(fixed-point (lambda (x) (+ (/ 1.0 x) 1)) 1.0)
