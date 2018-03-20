#lang eopl
;Exercise 2.29
(define-datatype lc-exp lc-exp?
  (var-exp (var identifier?))
  (lambda-exp
   (bound-var (list-of identifier?))
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand (list-of lc-exp?))))
(define identifier?
  (lambda (var)
    (if(symbol? var)
       (if (eq? var 'lambda) #f #t) #f)))

; parse-expression : SchemeVal -> LcExp
(define parse-expression
  (lambda (datum)
    (cond
      [(identifier? datum) (var-exp datum)]
      [(pair? datum)
       (if (eq? (car datum) 'lambda)
           (lambda-exp
            (cadr datum)
            (parse-expression (caddr datum)))
           (app-exp
            (parse-expression (car datum))
            (map parse-expression (cdr datum))))]
      [else
       (report-invalid-concrete-syntax datum)])))
(define report-invalid-concrete-syntax
  (lambda (datum)
    (eopl:error 'parse-expression
                "Wrong input of expression : ~s" datum)))

; Test
(define lc1 'a)
(define lc2 '(lambda (x) y))
(define lc3 (list 'lambda (list 'x 'y) (list 'x 'y)))