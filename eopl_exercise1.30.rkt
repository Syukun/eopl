#lang eopl
; Exercise 1.30
; sort/predicate : Predicate * list-of-Int -> list-of-Int
; usage : returns a list of elements sorted by the predicate.
; Example : (sort/predicate < '(8 2 5 2 3)) "should be" (2 2 3 5 8)
;           (sort/predicate > '(8 2 5 2 3)) "should be" (8 5 3 2 2)
(define sort/predicate
  (lambda (pred loi)
    (if (null? loi) '()
        (sort-predicate-auxi pred (car loi) (sort/predicate pred (cdr loi))))))
(define sort-predicate-auxi
  (lambda (pred i loi-sorted)
    (if (null? loi-sorted) (cons i loi-sorted)
        (if(pred i (car loi-sorted)) (cons i loi-sorted)
           (cons (car loi-sorted) (sort-predicate-auxi pred i (cdr loi-sorted)))))))