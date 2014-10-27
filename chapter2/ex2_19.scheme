(define us-coins (list 50 25 10 5 1))
(define cn-coins (list 100 50 20 10 5 1))

(define (first-demonination coin-values)
    (car coin-values))

(define (except-first-demonination coin-values)
    (cdr coin-values))

(define no-more? null?)

(define (cc amount coin-values)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (no-more? coin-values)) 0)
          (else 
            (+ (cc amount
                   (except-first-demonination coin-values))
               (cc (- amount (first-demonination coin-values))
                   coin-values)))))

(cc 100 us-coins)
