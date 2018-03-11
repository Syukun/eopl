#lang eopl
; representation one:
; Constructor:
; var-exp : Var -> LcExp
(define var-exp
  (lambda (var)
    (var)))
; lambda-exp : Var * LcExp -> LcExp
(define lambda-exp
  (lambda (var lcexp)
    (list 'lambda (var) lcexp)))
; app-exp : LcExp * LcExp -> LcExp
(define app-exp
  (lambda (lcexp1 lcexp2)
    (list lcexp1 lcexp2)))

; Predicate:
; var-exp? : Lc-exp -> Bool
(define var-exp?
  (lambda (lcexp)
    (if(symbol? lcexp) #t #f)))
; lambda-exp? : Lc-exp -> Bool
(define lambda-exp?
  (lambda (lcexp)
    (if (and (pair? lcexp)
             (eqv? (car lcexp) 'lambda))
        #t #f)))
; app-exp? : Lc-exp -> Bool
(define app-exp?
  (lambda (lcexp)
    (if (and (not (eqv? (car lcexp) 'lambda))
             (pair? lcexp))
        #t #f)))

; Extractor:
;var-exp->var : Lc-exp → Var
;(var-exp->var (var-exp 'a)) "should be" 'a
(define var-exp->var
  (lambda(lcexp)
    (lcexp)))
;lambda-exp->bound-var : Lc-exp → Var
(define lambda-exp->bound-var
  (lambda(lcexp)
    (caadr lcexp)))
;lambda-exp->body : Lc-exp → Lc-exp
(define lambda-exp->body
  (lambda(lcexp)
    (caddr lcexp)))
;app-exp->rator : Lc-exp → Lc-exp 
(define app-exp->rator
  (lambda(lcexp)
    (car lcexp)))
;app-exp->rand : Lc-exp → Lc-exp
(define app-exp->rand
  (lambda(lcexp)
    (cadr lcexp)))
; occurs-free? : Sym × LcExp → Bool
(define occurs-free?
  (lambda (search-var exp)
    (cond
      ((var-exp? exp) (eqv? search-var (var-exp->var exp)))
      ((lambda-exp? exp)
       (and
        (not (eqv? search-var (lambda-exp->bound-var exp)))
        (occurs-free? search-var (lambda-exp->body exp))))
      (else (or
             (occurs-free? search-var (app-exp->rator exp))
             (occurs-free? search-var (app-exp->rand exp)))))))
