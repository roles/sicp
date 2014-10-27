(define (make-point a b)
    (cons a b))

(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (print-point p)
    (newline)
    (display "(")
    (display (x-point p))
    (display ",")
    (display (y-point p))
    (display ")"))

; 起点和终点表示线段
(define (make-segment x1 y1 x2 y2)
    (cons (make-point x1 y1)
          (make-point x2 y2)))

(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (mid-segment s)
    (make-point (/ (+ (x-point (start-segment s))
                      (x-point (end-segment s)))
                   2.0)
                (/ (+ (y-point (start-segment s))
                      (y-point (end-segment s)))
                   2.0)))

(print-point (mid-segment (make-segment 1 2 3 6)))
