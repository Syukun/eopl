#lang eopl
; Exercise 1.12
; InLine usually used in optimizing compiler
(define subst_inline
  (lambda (new old slist)
    (if(null? slist)
       '()
       (cons
        (if (symbol? (car slist))
            (if (eqv? (car slist) old) new (car slist))
            (subst_inline new old (car slist)))
        (subst_inline new old (cdr slist))))))

