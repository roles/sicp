(define (inc n) (+ n 1))

; 丘奇计数
; 0表示x, 1表示(f x), 2表示(f (f x))
; f和x都是可以指定的,这里以inc和0替代
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
    (lambda (f) (lambda (x) (f ((n f) x)))))

; ((zero inc) 0)
; (((add-1 zero) inc) 0)

(define one
    (lambda (f) (lambda (x) (f x))))
(define two
    (lambda (f) (lambda (x) (f (f x)))))

; ((one inc) 0)
; ((two inc) 0)

(define (add a b)
    (lambda (f) (lambda (x) ((a f) ((b f) x)))))

; (((add one two) inc) 0)
