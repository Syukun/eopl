#lang eopl
; Exercise 1.22
; filter-in : Predicate * List -> List
; usage : returns the list of those elements in lst that satisfy the predicate pred.
; Example : (filter-in number? '(a 2 (1 3) b 7)) "should be" (2 7)
;           (filter-in symbol? '(a (b c) 17 foo)) "should be" (a foo)
(define filter-in
  (lambda (pred lst)
    (if (null? lst) '()
        (if (pred (car lst))
            (cons (car lst) (filter-in pred (cdr lst)))
            (filter-in pred (cdr lst))))))
