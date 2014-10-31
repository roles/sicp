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

(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
                   (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
    (make-interval (- (lower-bound x) (upper-bound y))
                   (- (upper-bound x) (lower-bound y))))

(define (div-interval x y)
    (if (<= (* (lower-bound y) (upper-bound y)) 0)
        (error "divide cross 0!") 
        (make-interval (/ (lower-bound x) (upper-bound y))
                       (/ (upper-bound x) (lower-bound y)))))

(define (mul-interval x y)
    (let ((x1 (lower-bound x))
          (x2 (upper-bound x))
          (y1 (lower-bound y))
          (y2 (upper-bound y)))
        (cond ((>= x1 0)
               (cond ((>= y1 0) (make-interval (* x1 y1) (* x2 y2)))
                     ((<= y2 0) (make-interval (* x2 y1) (* x1 y2)))
                     (else (make-interval (* x2 y1) (* x2 y2)))))
              ((<= x2 0)
               (cond ((>= y1 0) (make-interval (* x1 y2) (* x2 y1)))
                     ((<= y2 0) (make-interval (* x2 y2) (* x1 y1)))
                     (else (make-interval (* x1 y2) (* x1 y1)))))
              (else
               (cond ((>= y1 0) (make-interval (* x1 y2) (* x2 y2)))
                     ((<= y2 0) (make-interval (* x2 y1) (* x1 y1)))
                     (else (make-interval (min (* x1 y2) (* x2 y1)) (max (* x2 y2) (* x1 y1)))))))))

(define (print-interval p)
    (newline)
    (display (center p))
    (display "+-")
    (display (* 100 (percent p)))
    (display "%"))

(define (par1 r1 r2)
    (div-interval (mul-interval r1 r2)
                  (add-interval r1 r2)))

(define (par2 r1 r2)
    (let ((one (make-interval 1 1)))
        (div-interval one
                      (add-interval (div-interval one r1)
                                    (div-interval one r2)))))

(print-interval (par1 (make-center-percent 3 0.01) (make-center-percent 5 0.02)))
(print-interval (par2 (make-center-percent 3 0.01) (make-center-percent 5 0.02)))
