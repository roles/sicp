(define (last-pair n)
    (if (null? (cdr n))
        n
        (last-pair (cdr n))))

(last-pair (list 1 2 3 4))
