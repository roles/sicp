(define nil ())

(define (accumulate op init seqs)
    (if (null? seqs)
        init
        (op (car seqs)
            (accumulate op init (cdr seqs)))))

(define (map proc seq)
    (accumulate (lambda (x y) (cons (proc x) y)) nil seq))

(define (append list1 list2)
    (accumulate cons list2 list1))

(define (enumerate-interval x y)
    (if (> x y)
        nil
        (cons x (enumerate-interval (+ x 1) y))))

(define (flatmap proc seq)
    (accumulate append nil (map proc seq)))

(define (filter predicate sequence)
    (cond ((null? sequence) nil)
          ((predicate (car sequence))
           (cons (car sequence)
                 (filter predicate (cdr sequence))))
          (else (filter predicate (cdr sequence)))))

(define (unique-pairs n)
    (flatmap (lambda (x) (map (lambda (i) (list x i))
                              (enumerate-interval 1 (- x 1))))
             (enumerate-interval 1 n)))

(define (triple-pairs n)
    (flatmap (lambda (x) (map (lambda (i) (cons x i))
                              (unique-pairs (- x 1))))
             (enumerate-interval 1 n)))

(define (triple-sum n s)
    (filter (lambda (t) (= s (+ (car t) (cadr t) (caddr t))))
            (triple-pairs n)))
    
