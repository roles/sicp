(define (even? x) (= (remainder x 2) 0))

; 计算一个数的幂对另一个数取模
; 算法类似求幂的二分递归
(define (expmod base exp m) 
    (cond ((= exp 0) 1)
          ((even? exp) 
            (remainder (square (expmod base (/ exp 2) m)) m))
          (else 
            (remainder (* base (expmod base (- exp 1) m)) m))))

; 费马小定理，若n为素数，a<n，则 a^n % n == a 
(define (fermit-test n)
    (define (try-it a)
        (= (expmod a n n) a))
    (try-it (+ 1 (random (- n 1)))))

; 选择随机的a进行费马测试，能够过检查，则n为素数的机会就大于一半
(define (fast-prime? n times)
    (cond ((= times 0) true)
          ((fermit-test n) (fast-prime? n (- times 1)))
          (else false)))

(define (report-prime elasped-time)
    (display " *** ")
    (display elasped-time)
    true)
(define (start-prime-test n start-time)
    (if (fast-prime? n 10) ; 进行10次费马测试
        (report-prime (- (runtime) start-time))
        false))
(define (timed-prime-test n)
    (newline)
    (display n)
    (start-prime-test n (runtime)))

(define (search-for-primes n count)
    (define (next x) 
        (cond ((even? x) (+ x 1))
              (else (+ x 2))))
    (if (> count 0)
        (if (timed-prime-test n) 
            (search-for-primes (next n) (- count 1))
            (search-for-primes (next n) count)))
    true)
