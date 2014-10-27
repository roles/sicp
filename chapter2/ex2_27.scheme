(define nil ())

(define (reverse n)
    (define (reverse-iter n l)
        (if (null? n)
            l
            (reverse-iter (cdr n) (cons (car n) l))))
    (if (null? n)
        nil
        (reverse-iter (cdr n) (list (car n))))) ; 用list在第一个元素后面加nil

; 在reverse实现的基础上，对car items进行deep-reverse即可
; 注意car items可能为值类型，要用pair?进行判断
(define (deep-reverse items)
    (define (reverse-iter items result)
        (cond ((null? items) 
                result)
              ((not (pair? (car items))) 
                (reverse-iter (cdr items) (cons (car items) result)))
              (else 
                (reverse-iter (cdr items) (cons (deep-reverse (car items)) result)))))
    (cond ((null? items) nil)
          ((not (pair? (car items))) 
            (reverse-iter (cdr items) (list (car items))))
          (else 
            (reverse-iter (cdr items) (list (deep-reverse (car items)))))))
    

; (reverse (list (list 1 2) (list 3 4) (list 5 6)))
; (reverse (list (list 1 (list 2 3)) (list 4 (list 5 6))))
; (deep-reverse (list (list 1 2) (list 3 4) (list 5 6)))
; (deep-reverse (list (list 1 (list 2 3)) (list 4 (list 5 6))))
