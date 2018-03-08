#lang eopl
; Exercise 1.25
; exist? : Predicate * List -> Boolean
; usage : returns #t if any element of lst satisfies pred, and returns #f otherwise.
; Example : (exist? number? '(a b c 3 e)) "should be" #t
;           (exist? number? '(a b c d e)) "should be" #f
(define exist?
  (lambda (pred lst)
    (if (null? lst) #f
        (if (pred (car lst)) #t
            (exist? pred (cdr lst))))))