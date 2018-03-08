#lang eopl
; Exercise 1.23
; list-index : Predicate * List -> Int or #f
; usage : returns the 0-based position of the first element of lst that satisfies the predicate pred.
;         If no element of lst satisfies the predicate, then list-index returns #f.
; Example : (list-index number? '(a 2 (1 3) b 7)) "should be" 1
;           (list-index symbol? '(a (b c) 17 foo)) "should be" 0
;           (list-index symbol? '(1 2 (a b) 3)) "should be" #f
(define list-index
  (lambda (pred lst)
    (list-index-auxi pred lst 0)))
(define list-index-auxi
  (lambda (pred lst n)
    (if (null? lst) #f
        (if (pred (car lst)) n
            (list-index-auxi pred (cdr lst) (+ 1 n))))))
