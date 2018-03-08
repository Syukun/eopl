#lang eopl
; Exercise 1.7
; nth-element-modified : List * Int * List * Int -> SchemeVal
; usage: return error message such as "(a b c) does not have 8 elements."
; Example : (nth-element-modified lst_t1 2 lst_t1 2) "should be" c
(define nth-element-modified
  (lambda (lst n ori-lst ori-num)
    (if (null? lst)
        (report-list-too-short-modified ori-lst ori-num)
        (if(zero? n)
           (car lst)
           (nth-element-modified (cdr lst) (- n 1) ori-lst ori-num)))))
(define report-list-too-short-modified
  (lambda (lst num)
    (eopl:error 'nth-element-modified
                "~s does not have ~s elements.~%" lst num)))

; test
(define lst_t1 '(a b c))