(define nil ())

(define (map proc items)
    (if (null? items)
        nil
        (cons (proc (car items))    ; 递归构造list
              (map proc (cdr items)))))

(define (square-list-a items)
    (if (null? items)
        nil
        (cons (square (car items))
              (square-list-a (cdr items)))))

(square-list-a (list 1 2 3 4 5))

(define (square-list-b items)
    (map square items))

(square-list-b (list 1 2 3 4 5))
