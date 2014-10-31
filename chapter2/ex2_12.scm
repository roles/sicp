(define (make-interval a b) (cons (min a b) (max a b)))
(define (lower-bound z) (car z))
(define (upper-bound z) (cdr z))

(define (make-center-percent c p)
    (make-interval (- c (* c p)) (+ c (* c p))))

(define (center i)
    (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
    (/ (- (upper-bound i) (lower-bound i)) 2))

(define (percent i)
    (/ (width i) (center i)))
