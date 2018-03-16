#lang eopl
; Exercise 2.1
; Implement four operations in bigits
(define zero (lambda() '()))
(define is-zero? (lambda(n) (zero? n)))
; (successor '(1 0 1 0 1)) "should be (0 1 1 0 1)

(define  successor
  (lambda(n)
    (if (null? n) (cons 1 empty)
        (if (eqv? (car n) 0)
            (cons 1 (cdr n))
            (cons 0 (successor (cdr n)))))))
(define predecessor
  (lambda(n)
    (if (zero? n) report-short-number
        (if (eqv? (car n) 1)
            (cons 0 (cdr n))
            (cons 1 (predecessor (cdr n)))))))
(define report-short-number
  (lambda()
    (eopl:error 'prodecessor
                "0 don't have predecessor number.~%")))
; Exercise 2.3
; Diff-tree ::= (one) | (diff Diff-tree Diff-tree)
; 1. because if the value is v ,
;    then the first value is v , the value of second parameter can be ゼロ
; 2
(define zero-diff
  (lambda()
    (list 'diff (list 'one) (list 'one))))
(define is-zero?-diff
  (lambda(dt)
    (if (null? (cdr dt)) #f
        (eqv? (eval-diff (cadr dt))
              (eval-diff (caddr dt))))))
(define eval-diff
  (lambda(dt)
    (if (null? (cdr dt)) 1
        (- (eval-diff (cadr dt))
           (eval-diff (caddr dt))))))
(define successor-diff
  (lambda (dt)
    (list 'diff dt (list 'diff (list 'diff zero-diff (list 'one))))))
(define prodecessor-diff
  (lambda (dt)
    (list 'diff dt (list 'one))))
; 3.
; diff-tree-plus :
(define diff-tree-plus
  (lambda(dt1 dt2)
    (list 'diff dt1 (list 'diff zero-diff dt2))))

;Exercise 2.4
; constructor:
; empty-stack = [ø]
; (push v [s]) =[v.s]
; (pop [v.s]) = [s]
; observers:
; (top [v.s]) = [v]
; (empty-stack? s) = #t if s =[ø],else #f

; Exercise 2.5
; empty-env : ()
(define empty-env '())
; extend-env : Var * SchemeVal * Env -> Env
(define extend-env
  (lambda (var val env)
    (cons (cons var val) env)))
; apply-env : Env * Var -> SchemeVal
(define apply-env
  (lambda (env search-var)
    (cond
      [(null? env) (report-no-binding-found search-var)]
      [(pair? env)
       (let ((saved-var (caar env))
             (saved-val (cdar env))
             (saved-env (cdr env)))
         (if (eqv? search-var saved-var)
             saved-val
             (apply-env saved-env search-var)))]
      [else  (report-invalid-env env)])))
(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env
                "No binding for ~s" search-var)))
(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env
                "Bad environment: ~s" env)))
;test
(define env_t
  (extend-env 'a 1 (extend-env 'b 2 (extend-env 'c 3 empty-env))))

; Exercise 2.6
; Representation 1 : a-list or association-list representation which is represented in Exercise 2.5
; Representation 2 : shape like ((a b c d) (1 2 3 4)) empty-env is (()())
; Representation 3 : shape like binary search tree 
;                    (cons (b 2) (a 1) (cons (c 3) () (d 4)))

; Exercise 2.7
(define apply-env-more-information
  (lambda (env search-var ori-env)
    (cond
      [(eqv? (car env) 'empty-env)
       (report-no-binding-found-2 search-var ori-env)]
      [(eqv? (car env) 'extend-env)
       (let ((saved-var (cadr env))
             (saved-val (caddr env))
             (saved-env (cadddr env)))
         (if (eqv? search-var saved-var)
             saved-val
             (apply-env-more-information saved-env search-var ori-env)))]
      [else
       (report-invalid-env-2 env)])))
(define report-no-binding-found-2
  (lambda (search-var ori-env)
    (eopl:error 'apply-env-more-information
                "No binding for ~s in env ~s" search-var ori-env)))
(define report-invalid-env-2
  (lambda (env)
    (eopl:error 'apply-env-more-information
                "Bad environment: ~s" env)))

; Exercise 2.8
; empty-env? : Env -> Boolean
(define empty-env?
  (lambda (env)
    (if (null? env) #t
        #f)))

; Exercise 2.9
; has-binding? : Env * Sym -> Bool
; usage : return #t if s has an associated value in env.
; Example : (has-binding? env_t 'b) "should be" #t
(define has-binding?
  (lambda (env s)
    (if (empty-env? env) #f
        (let [(first-var (caar env))
              (smaller-env (cdr env))]
          (if (eqv? first-var s) #t
              (has-binding? smaller-env s))))))

; Exercise 2.10
; extend-env* : List-of-var * List-of-val * Env -> Env
(define extend-env*
  (lambda (lovar loval env)
    (if (null? lovar)
        env
        (let ((var1 (car lovar))
              (val1 (car loval)))
          (extend-env* (cdr lovar)
                       (cdr loval)
                       (extend-env var1 val1 env))))))

