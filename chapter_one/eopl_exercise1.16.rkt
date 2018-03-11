#lang eopl
; Exercise 1.16
; invert : list-of-2-list -> list-of-2-list
; usage : return a list with each 2-list reversed
; example : (invert '((a 1) (a 2) (1 b) (2 b))) "should be" ((1 a) (2 a) (b 1) (b 2))
(define invert
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (reverse (car lst))
              (invert (cdr lst))))))
(define reverse
  (lambda (2-list)
    (cons (cadr 2-list)
          (cons (car 2-list) empty))))
