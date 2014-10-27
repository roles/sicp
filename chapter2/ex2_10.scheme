(define (make-interval a b) (cons (min a b) (max a b)))
(define (lower-bound z) (car z))
(define (upper-bound z) (cdr z))

(define (print-interval p)
    (newline)
    (display "(")
    (display (lower-bound p))
    (display ",")
    (display (upper-bound p))
    (display ")"))

(define (divide-interval x y)
    (if (<= (* (lower-bound y) (upper-bound y)) 0)
        (error "divide cross 0!") 
        (make-interval (/ (lower-bound x) (upper-bound y))
                       (/ (upper-bound x) (lower-bound y)))))

(print-interval (divide-interval (make-interval 4 8) (make-interval 1 3)))
(print-interval (divide-interval (make-interval 4 8) (make-interval -1 1)))

