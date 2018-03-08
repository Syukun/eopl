#lang eopl
; Exercise 1.24
; every? : Predicate * List -> Boolean
; usage : returns #f if any element of lst fails to satisfy pred, and returns #t otherwise.
; Example : (every? number? '(a b c 3 e)) "should be" #f
;           (every? number? '(1 2 3 5 4)) "should be" #t
(define every?
  (lambda (pred lst)
    (if (null? lst) #t
        (if (pred (car lst)) (every? pred (cdr lst))
            #f))))
