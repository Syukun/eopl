#lang eopl

; Exercise 1.18
; swapper : Sym * Sym * s-list -> s-list
; usage : returns a list the same as slist, but
; with all occurrences of s1 replaced by s2
; and all occurrences of s2 replaced by s1.
; Example : (swapper 'a 'd '(a b c d)) "should be" (d b c a)
; (swapper 'a 'd '(a d () c d)) "should be" (d a () c a)
; (swapper 'x 'y '((x) y (z (x)))) "should be" ((y) x (z (y)))
(define swapper
  (lambda (s1 s2 slist)
    (if (null? slist)
        '()
        (cons (swapper-s-exp s1 s2 (car slist))
              (swapper s1 s2 (cdr slist))))))
(define swapper-s-exp
  (lambda (s1 s2 s-exp)
    (if (symbol? s-exp)
        (cond
          ((eqv? s-exp s1) s2)
          ((eqv? s-exp s2) s1)
          (else s-exp))
        (swapper s1 s2 s-exp))))
