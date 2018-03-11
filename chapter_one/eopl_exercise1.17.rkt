#lang eopl
; Exercise 1.17
; down : list-of-value -> list-of-(value)
; usage : wrap parentheses around each top-level element of lst
; example : (down '(1 2 3)) "should be" ((1) (2) (3))
; (down '((a) (fine) (idea))) "should be" (((a)) ((fine)) ((idea)))
; (down '(a (more (complicated)) object)) "should be" ((a) ((more (complicated))) (object))
(define down
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (cons (car lst) empty) (down (cdr lst))))))
