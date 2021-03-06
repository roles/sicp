(define nil ())

(define (filter predicate sequence)
    (cond ((null? sequence) nil)
          ((predicate (car sequence))
           (cons (car sequence)
                 (filter (predicate (cdr sequence)))))
          (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

; op中第二项为后面accumulate的结果，一般只需处理第一项
(define (map p sequence)
    (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
    (accumulate cons seq2 seq1))

(define (length sequence)
    (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

; 用accumulate递归实现count-leaves
(define (count-leaves t)
    (accumulate + 0 (map (lambda (x)
                            (if (pair? x)
                                (count-leaves x)
                                1))
                         t)))
