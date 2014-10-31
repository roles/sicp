(define (make-interval a b) (cons a b))
(define (lower-bound z) (min (car z) (cdr z)))
(define (upper-bound z) (max (car z) (cdr z)))
