#lang eopl

; Exercise 1.26
; ListOrNot ::= ListOf(SchemeVal) | Int | Symbol
; up : ListOf(ListOrNot) -> ListOf(SchemeVal)
; usage : removes a pair of parentheses from each top-level element of lst.
;         If a top-level element is not a list, it is included in the result, as is.
;         The value of (up (down lst)) is equivalent to lst, but (down (up lst)) is not necessarily lst.
; Example : (up '((1 2) (3 4))) "should be" (1 2 3 4)
;           (up '((x (y)) z)) "should be" (x (y) z)
(define up
  (lambda (lst)
    (cond ((null? lst) '())
          ((list? (car lst)) (append (car lst) (up (cdr lst))))
          (else (cons (car lst) (up (cdr lst)))))))
