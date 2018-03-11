#lang eopl
; Exercise 1.20
; count-occurrences : Symbol * slist -> Int
; usage : returns the number of occur- rences of s in slist.
; Example : (count-occurrences 'x '((f x) y (((x z) x)))) "should be" 3
;           (count-occurrences 'x '((f x) y (((x z) () x)))) "should be" 3
;           (count-occurrences 'w '((f x) y (((x z) x)))) "should be" 0
(define count-occurrences
  (lambda (s slist)
    (if (null? slist)
        0
        (+ (count-occurrences-s-exp s (car slist))
           (count-occurrences s (cdr slist))))))
(define count-occurrences-s-exp
  (lambda (s s-exp)
    (if (symbol? s-exp)
        (if (eqv? s s-exp) 1 0)
        (count-occurrences s s-exp))))
