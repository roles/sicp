(define nil ())

(define (same-partity first . other)
    (define (same? x) (= (remainder first 2) (remainder x 2)))
    (define (same-recur n)
        (cond ((null? n) nil)
              ((same? (car n)) (cons (car n) (same-recur (cdr n)))) ; 用递归cons来构造list
              (else (same-recur (cdr n)))))
    (cons first (same-recur other)))

(same-partity 1 2 3 4 5 6 7)
