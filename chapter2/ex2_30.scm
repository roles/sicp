(define nil ())

(define (map proc items)
    (if (null? items)
        nil
        (cons (proc (car items))
              (map proc (cdr items)))))

(define (square-tree tree)
    (define (map-proc proc) ; 对proc改造，使之能够递归
        (lambda (tree)
            (if (pair? tree)
                (square-tree tree)
                (proc tree))))
    (map (map-proc square) tree))

(square-tree (list 1
                   (list 2 (list 3 4) 5)
                   (list 6 7)))
