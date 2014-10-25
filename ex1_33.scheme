(define (filtered-accumulate combiner filter null-value term a next b) ; 迭代计算过程
    (define (iter a result) 
        (cond ((> a b) result)
              ((filter a) (iter (next a) (combiner (term a) result)))
              (else (iter (next a) result))))
    (iter a null-value))

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
          ((= n 1) false)
          ((fermit-test n) (fast-prime? n (- times 1)))
          (else false)))

(define (inc x) (+ x 1))
(define (identity x) x)
(define (add a b) (+ a b))

; 计算a到b之间素数之和
(define (prime-sum a b)
    (define (prime? n) (fast-prime? n 10))
    (filtered-accumulate add prime? 0 identity a inc b))

; 欧几里德法求最大公约数
; gcd(a, b) = gcd(b, r), r为a取模b
(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))

(define (mul a b) (* a b))

; 小于n互质的正整数乘积
(define (prime-prod n)
    (define (prime? x) (= (gcd n x) 1))
    (filtered-accumulate mul prime? 1 identity 1 inc (- n 1)))
