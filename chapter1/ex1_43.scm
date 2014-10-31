(define (compose f g)
    (lambda (x) (f (g x))))

; 使用之前的compose来实现
(define (repeated f n)
    (if (= n 1)
        f
        (compose f (repeated f (- n 1)))))

((repeated square 2) 5)
