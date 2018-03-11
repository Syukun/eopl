#lang eopl
; Exercise 1.15
; duple : Int * SchemeValue -> list-of-scheme-value
; usage : returns a list containing n copies of x
; example : (duple 2 3) "should be" (3 3)
; (duple 4 '(ha ha)) "should be" '((ha ha) (ha ha) (ha ha) (ha ha))

(define duple
  (lambda (n x)
    (if (zero? n)
        '()
        (cons x (duple (- n 1) x)))))
