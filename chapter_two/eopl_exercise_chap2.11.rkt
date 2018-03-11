#lang eopl
; Exercise 2.11
(define empty-env
  (lambda() '()))

(define extend-env
  (lambda (var val env)
    (cons (list
           (list var) (list val))
          env)))
(define extend-env*
  (lambda (lovar loval env)
    (if (null? lovar) env
        (cons (list lovar loval) env))))
(define apply-env
  (lambda (env search-var)
    (if (null? env) (report-no-binding-found search-var)
        (if (contains? (caar env) search-var)
            (apply-rib (caar env) (cadar env) search-var)
            (apply-env
             (cdr env) search-var)))))
(define contains?
  (lambda (lovar search-var)
    (if (null? lovar) #f
        (if (eqv? (car lovar) search-var) #t
            (contains? (cdr lovar) search-var)))))
(define apply-rib
  (lambda (lovar loval search-var)
    (if(eqv? (car lovar) search-var) (car loval)
       (apply-rib (cdr lovar) (cdr loval) search-var))))
(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env
                "No binding for ~s" search-var)))
(define env_t (extend-env* '(a b c) '(11 12 13) (extend-env* '(x z) '(66 77) (extend-env* '(x y) '(88 99) (empty-env)))))