(require (lib "racket/draw"))
(require racket/class)

(define target (make-bitmap 100 100))
(define dc (new bitmap-dc% [bitmap target]))

(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

(define (add-vect v1 v2)
    (make-vect (+ (xcor-vect v1) (xcor-vect v2))
               (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
    (make-vect (- (xcor-vect v1) (xcor-vect v2))
               (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
    (make-vect (* s (xcor-vect v))
               (* s (ycor-vect v))))

(define (make-segment start end) (cons start end))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (make-frame origin edge1 edge2)
    (list origin edge1 edge2))

(define (origin-frame f) (car f))
(define (edge1-frame f) (cadr f))
(define (edge2-frame f) (caddr f))

(define (frame-coord-map frame)
    (lambda (v)
        (add-vect 
            (origin-frame frame)
            (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
                      (scale-vect (ycor-vect v) (edge2-frame frame))))))

(define (drawline start end)
    (send dc draw-line (xcor-vect start) (ycor-vect start) 
             (xcor-vect end) (ycor-vect end)))

(define (segments-painter segment-list)
    (lambda (frame)
        (for-each
            (lambda (segment)
                (drawline 
                    ((frame-coord-map frame) (start-segment segment))
                    ((frame-coord-map frame) (end-segment segment))))
            segment-list)))

; (send target save-file "ex.png" 'png)
