#lang eopl
; first is apply-env, second is empty-env? , third is has-binding?
(define empty-env
  (lambda()
    (list (lambda (search-var) (report-no-binding-found search-var))
          (lambda () #t)
          (lambda () #f))))
(define extend-env
  (lambda(saved-var saved-val saved-env)
    (list (lambda (search-var)
            (if(eqv? search-var saved-var) saved-val
               (apply-env saved-var search-var)))
          (lambda ()
            #f)
          (lambda (search-var)
            (if (eqv? search-var saved-var) #t
                (has-binding? saved-env search-var))))))
(define apply-env
  (lambda (env search-var)
    ((car env) search-var)))
(define empty-env?
  (lambda (env)
    ((cadr env))))
(define has-binding?
  (lambda (env search-var)
    ((caddr env) search-var)))
(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))