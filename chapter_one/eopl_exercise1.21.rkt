#lang eopl
; Exercise 1.21
; product : list-of-symbol * list-of-symbol -> list-of-2-lists
; usage : returns a list of 2-lists that represents the Cartesian product of sos1 and sos2. The 2-lists may appear in any order.
; Example : (product '(a b c) '(x y)) "should be" ((a x) (a y) (b x) (b y) (c x) (c y))
(define product
  (lambda (sos1 sos2)
    (if (null? sos1) '()
        (append (product-auxi (car sos1) sos2)
                (product (cdr sos1) sos2)))))
(define product-auxi
  (lambda (s sos)
    (if (null? sos) '()
        (cons (cons s (cons (car sos) empty))
              (product-auxi s (cdr sos))))))
