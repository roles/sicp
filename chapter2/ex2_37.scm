(define nil ())

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

; 对矩阵列进行累计操作
(define (accumulate-n op init seqs)
    (if (null? (car seqs))
        nil
        (cons (accumulate op init (map (lambda (x) (car x)) seqs))
              (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))

; 这里用到了scheme的标准map，可以接受多个list
(define (dot-product v w)
    (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
    (map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
    (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
    (let ((cols (transpose n)))
        (map (lambda (v) (matrix-*-vector cols v)) m)))

(define v (list 1 2 3))
(define w (list 1 3 5))
(define m (list (list 1 2 3) (list 4 5 6)))
(define n (list (list 1 1 1 1) (list 2 2 2 2) (list 3 3 3 3)))

