#lang eopl
(define-datatype lc-exp lc-exp?
  (var-exp (var identifier?))
  (lambda-exp
   (bound-var identifier?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))
(define identifier?
  (lambda (var)
    (if (symbol? var)
        (if (eq? var 'lambda)
            #f #t)
        #f)))
;parse-expression ; SchemeVal -> LcExp
(define parse-expression
  (lambda (datum)
    (cond
      [(symbol? datum)
       (if (identifier? datum) (var-exp datum)
           (report-lambda-key-invalid datum))]
      [(eq? (length datum 3))
       (if (eqv? (car datum) 'lambda)
           (if (identifier? (car (cadr datum)))
               (lambda-exp
                (car (cadr datum))
                (parse-expression (caddr datum)))
               (report-lambda-key-invalid datum))
           (report-invalid-concrete-syntax datum))]
[(eq? (length datum 2))
 (app-exp
  (parse-expression (car datum))
  (parse-expression (cadr datum)))]
[else (report-invalid-concrete-syntax datum)])))
(define report-lambda-key-invalid
  (lambda (datum)
    (eopl:error 'parse-expression
                "lambda is a keyword ,
there for can't be used as an identifier in expression ~s" datum)))
(define report-invalid-concrete-syntax
  (lambda (datum)
    (eopl:error 'parse-expression
                "Input ~s is in a wrong form." datum)))