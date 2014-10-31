(define nil ())

(define (append list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (append (cdr list1) list2))))

(define (fringe tree)
    (cond ((null? tree) nil)
          ((not (pair? tree)) (list tree))
          (else (append (fringe (car tree)) (fringe (cdr tree))))))
