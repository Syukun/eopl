#lang eopl
; Constructor:
; var-exp : Var -> LcExp
(define var-exp
  (lambda (var)
    (list 'var-exp var)))
; lambda-exp : Var * LcExp -> LcExp
(define lambda-exp
  (lambda (var lcexp)
    (list 'lambda-exp 'lambda (var) lcexp)))
; app-exp : LcExp * LcExp -> LcExp
(define app-exp
  (lambda (lcexp1 lcexp2)
    (list 'app-exp lcexp1 lcexp2)))

; Predicate:
; var-exp? : Lc-exp -> Bool
(define var-exp?
  (lambda (lcexp)
    (if(eqv? (car lcexp) 'var-exp) #t #f)))
; lambda-exp? : Lc-exp -> Bool
(define lambda-exp?
  (lambda (lcexp)
    (if (eqv? (car lcexp) 'lambda-exp) #t #f)))
; app-exp? : Lc-exp -> Bool
(define app-exp?
  (lambda (lcexp)
    (if (eqv? (car lcexp) 'app-exp) #t #f)))

; Extractor:
;var-exp->var : Lc-exp → Var
;(var-exp->var (var-exp 'a)) "should be" 'a
(define var-exp->var
  (lambda(lcexp)
    (cadr lcexp)))
;lambda-exp->bound-var : Lc-exp → Var
(define lambda-exp->bound-var
  (lambda(lcexp)
    (caaddr lcexp)))
;lambda-exp->body : Lc-exp → Lc-exp
(define lambda-exp->body
  (lambda(lcexp)
    (cadddr lcexp)))
;app-exp->rator : Lc-exp → Lc-exp 
(define app-exp->rator
  (lambda(lcexp)
    (cadr lcexp)))
;app-exp->rand : Lc-exp → Lc-exp
(define app-exp->rand
  (lambda(lcexp)
    (caddr lcexp)))
; occurs-free? : Sym × LcExp → Bool
(define occurs-free?
  (lambda (search-var exp)
    (cond
      ((var-exp? exp) (eqv? search-var (var-exp->var exp))) ((lambda-exp? exp)
                                                             (and
                                                              (not (eqv? search-var (lambda-exp->bound-var exp))) (occurs-free? search-var (lambda-exp->body exp))))
      (else (or
             (occurs-free? search-var (app-exp->rator exp)) (occurs-free? search-var (app-exp->rand exp)))))))