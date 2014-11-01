(define nil ())

(define (fold-right op init seqs)
    (if (null? seqs)
        init
        (op (car seqs)
            (fold-right op init (cdr seqs)))))

(define (fold-left op init seqs)
    (define (iter result rest)
        (if (null? rest)
            result
            (iter (op result (car rest))
                  (cdr rest))))
    (iter init seqs))

(define (append list1 list2)
    (fold-right cons list2 list1))

(define (reverse sequence)
    (fold-right (lambda (x y) (append y (list x))) nil sequence))

(define (reverse2 sequence)
    (fold-left (lambda (x y) (cons y x)) nil sequence))
