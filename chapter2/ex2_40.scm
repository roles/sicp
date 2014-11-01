(define nil ())

(define (accumulate op init seqs)
    (if (null? seqs)
        init
        (op (car seqs)
            (accumulate op init (cdr seqs)))))

(define (map proc seq)
    (accumulate (lambda (x y) (cons (proc x) y)) nil seq))

(define (append list1 list2)
    (accumulate cons list2 list1))

(define (enumerate-interval x y)
    (if (> x y)
        nil
        (cons x (enumerate-interval (+ x 1) y))))

(define (flatmap proc seq)
    (accumulate append nil (map proc seq)))

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

(define (prime? n)
    (fast-prime? n 10))

(define (unique-pairs n)
    (flatmap (lambda (x) (map (lambda (i) (list x i))
                              (enumerate-interval 1 (- x 1))))
             (enumerate-interval 1 n)))

(define (filter predicate sequence)
    (cond ((null? sequence) nil)
          ((predicate (car sequence))
           (cons (car sequence)
                 (filter predicate (cdr sequence))))
          (else (filter predicate (cdr sequence)))))

(define (prime-sum-pairs n)
    (map (lambda (p) (list (car p) (cadr p) (+ (car p) (cadr p))))
         (filter (lambda (p) (prime? (+ (car p) (cadr p))))
                 (unique-pairs n))))

