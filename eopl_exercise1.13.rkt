#lang eopl
; Exercise 1.13
(define subst-map
  (lambda (new old slist)
    (map (lambda (sexp) (subst-in-s-exp-map new old sexp)) slist)))
(define subst-in-s-exp-map
  (lambda (new old sexp)
    (if (symbol? sexp)
        (if (eqv? sexp old) new sexp)
        (subst-map new old sexp))))
