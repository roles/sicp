(define nil ())

(define (append list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (append (cdr list1) list2))))

(define (map proc items)
    (if (null? items)
        nil
        (cons (proc (car items))
              (map proc (cdr items)))))

; rest表示(cdr s)的元素所能构成的集合列表
; 将rest和(car s)添加到每一个rest的两个列表组合即可
(define (subsets s)
    (if (null? s)
        (list nil)
        (let ((rest (subsets (cdr s))))
            (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subsets (list 1 2 3))
