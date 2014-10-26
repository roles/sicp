(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))

(define (make-rat a b)
    (let ((g (gcd a b)))
        (if (< (/ b g) 0)
            (cons (- 0 (/ a g)) (- 0 (/ b g)))
            (cons (/ a g) (/ b g)))))

(define (numer x) (car x))
(define (demon x) (cdr x))
(define (print-rat x)
    (newline)
    (display (numer x))
    (display "/")
    (display (demon x)))

(print-rat (make-rat 18 -15))
(print-rat (make-rat -18 -15))
(print-rat (make-rat -18 15))
(print-rat (make-rat 18 15))
