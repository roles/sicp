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

(define (adjoin-position new-row k rest-of-queens)
    (cons new-row rest-of-queens))

(define empty-board nil)

(define (safe? k position)
    (define (iter row-k i position)
        (cond ((= 0 i) true)
              ((or (= (car position) row-k)
                   (= (- k i) (abs (- row-k (car position)))))
               false)
              (else (iter row-k (- i 1) (cdr position)))))
    (iter (car position) (- k 1) (cdr position)))

(define (queens board-size)
    (define (queens-cols k)
        (if (= k 0)
            (list empty-board)
            (filter 
                (lambda (position) (safe? k position))
                (flatmap ; 针对当前可行布局扩展第k+1列
                    (lambda (rest-of-queens) ; rest-of-queens是一个k列的可行布局
                        (map (lambda (new-row) ; new-row是第k+1列的一个新尝试
                                (adjoin-position new-row k rest-of-queens))
                             (enumerate-interval 1 board-size))) ; 对所有1到board-size行都进行尝试
                    (queens-cols (- k 1))))))
    (queens-cols board-size))
