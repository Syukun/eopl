#lang eopl
; Exercise 1.29
; sort : list-of-Int -> list-of-Int
; usage : returns a list of the elements of loi in ascending order.
; Example : (sort '(8 2 5 2 3)) "should be" (2 2 3 5 8)
(define sort
  (lambda (loi)
    (if (null? loi) '()
        (sort-auxi (car loi) (sort (cdr loi))))))
(define sort-auxi
  (lambda (i loi-sorted)
    (if(null? loi-sorted) (cons i loi-sorted)
       (if(< i (car loi-sorted)) (cons i loi-sorted)
          (cons (car loi-sorted) (sort-auxi i (cdr loi-sorted)))))))
