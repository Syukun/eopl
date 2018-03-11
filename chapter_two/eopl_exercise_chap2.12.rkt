#lang eopl
(define empty-stack
  (lambda()
    (lambda (command)
      (cond [(eqv? command 'empty-stack?) #t]
            [(eqv? command 'top) (report-out-of-bound)]
            [(eqv? command 'pop) (report-out-of-bound)]))))
(define push
  (lambda(stack ele)
    (lambda (command)
      (cond [(eqv? command 'empty-stack?) #f]
            [(eqv? command 'top) ele]
            [(eqv? command 'pop) stack]))))
(define pop
  (lambda (stack)
    (stack 'pop)))
(define top
  (lambda (stack)
    (stack 'top)))
(define empty-stack?
  (lambda (stack)
    (stack 'empty-stack?)))
(define report-out-of-bound
  (lambda ()
    (eopl:error 'empty-stack
                "Index out of bound")))
;test
(eq? (empty-stack? (empty-stack)) #t)
(eq? (top (push (empty-stack) 'a)) 'a)
(eq? (empty-stack? (pop (push (empty-stack) 'a))) #t)