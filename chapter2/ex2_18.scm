(define nil ())

(define (reverse n)
    (define (reverse-iter n l)
        (if (null? n)
            l
            (reverse-iter (cdr n) (cons (car n) l))))
    (if (null? n)
        nil
        (reverse-iter (cdr n) (list (car n))))) ; 用list在第一个元素后面加nil

(reverse (list 1 2 3 4 5))
