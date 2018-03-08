#lang eopl
; Exercise 1.27
; flatten : slist -> List
; usage :returns a list of the symbols contained in slist in the order in which they occur when slist is printed.
;        Intuitively, flatten removes all the inner parentheses from its argument.
; Example : (flatten '(a b c)) "should be" (a b c)
;           (flatten '((a) () (b ()) () (c))) "should be" (a b c)
;           (flatten '((a b) c (((d)) e))) "should be" (a b c d e)
;           (flatten '(a b (() (c)))) "should be" (a b c)
(define flatten
  (lambda (slist)
    (if (null? slist) '()
        (if(not (null? (flatten-s-exp (car slist))))
           (append (flatten-s-exp (car slist))
                   (flatten (cdr slist)))
           (flatten (cdr slist))))))
(define flatten-s-exp
  (lambda (s-exp)
    (if (symbol? s-exp) (cons s-exp empty)
        (flatten s-exp))))