#lang eopl
(define-datatype lc-exp lc-exp?
  (var-exp (var var?))
  (lambda-exp
   (bound-var symbol?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))
(define var?
  (lambda (var)
    (if (symbol? var)
        (if (eq? var 'lambda) #f #t)
        #f)))
              
; unparse-lc-exp : LcExp -> String
(define unparse-lc-exp
  (lambda (exp)
    (cases lc-exp exp
      (var-exp (var) (symbol->string var))
      (lambda-exp (bound-var body)
                  (string-append "(lambda (" (symbol->string bound-var) ") " 
                                 (unparse-lc-exp body) ")" ))
      (app-exp (rator rand)
               (string-append "(" (unparse-lc-exp rator) " " (unparse-lc-exp rand) ")" )))))

; Test
(define lc1 (var-exp 'a))
(define lc2 (lambda-exp 'x (var-exp 'y)))
(define lc3 (app-exp (var-exp 'x) (var-exp 'x))) 