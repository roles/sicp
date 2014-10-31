(define (make-mobile left right)
    (list left right))

(define (left-branch mobile)
    (car mobile))

(define (right-branch mobile)
    (car (cdr mobile)))

(define (make-branch length structure)
    (list length structure))

(define (branch-length branch)
    (car branch))

(define (branch-structure branch)
    (car (cdr branch)))

(define (total-weight mobile)
    (if (not (pair? mobile))
        mobile
        (+ (total-weight (branch-structure (left-branch mobile)))
           (total-weight (branch-structure (right-branch mobile))))))

(define (balance? mobile)
    (if (not (pair? mobile))
        true
        (and (balance? (branch-structure (left-branch mobile)))
             (balance? (branch-structure (right-branch mobile)))
             (= (* (total-weight (branch-structure (left-branch mobile)))
                   (branch-length (left-branch mobile)))
                (* (total-weight (branch-structure (right-branch mobile)))
                   (branch-length (right-branch mobile)))))))

(define m (make-mobile (make-branch 2 (make-mobile (make-branch 3 2)
                                                   (make-branch 1 6)))
                       (make-branch 4 4)))
; (balance? (make-mobile (make-branch 1 1) (make-branch 2 2))) 
