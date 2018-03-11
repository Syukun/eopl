#lang eopl
; Exercise 1.28
; merge : list-of-int * list-of-int -> list-of-int
; usage : returns a sorted list of all the integers in loi1 and loi2.
;         where loi1 and loi2 are lists of integers that are sorted in ascending order
; Example : (merge '(1 4) '(1 2 8)) "should be" (1 1 2 4 8)
;           (merge '(35 62 81 90 91) '(3 83 85 90)) "should be" (3 35 62 81 83 85 90 90 91)
(define merge
  (lambda (loi1 loi2)
    (cond ((and (null? loi1) (null? loi2)) '())
          ((null? loi1) loi2)
          ((null? loi2) loi1)
          (else
           (if(< (car loi1) (car loi2))
              (cons (car loi1) (merge (cdr loi1) loi2))
              (cons (car loi2) (merge loi1 (cdr loi2))))))))
