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

; (fold-right / 1 (list 1 2 3))
; (fold-left / 1 (list 1 2 3))
; (fold-right list nil (list 1 2 3))
; (fold-left list nil (list 1 2 3))
