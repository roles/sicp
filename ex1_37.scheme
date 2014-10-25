; 计算k项有限连分式
;         N_1
; __________________
;          N_2
; D_1+ _____________
;       D_2 + ...
;
(define (cont-frac n d k)
    (define (frac i prev)
        (/ (n i) (+ (d i) prev)))
    (define (iter i result)
        (if (= i 0)
            result
            (iter (- i 1) (frac i result))))
    (iter k 0))

; Ni Di都取1,则会逼近黄金分割率的倒数
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           100)
