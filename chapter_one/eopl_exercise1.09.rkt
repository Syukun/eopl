#lang eopl
; Exercise 1.9
; remove : Symbol * ListOf(Symbol) -> ListOf(Symbol)
(define remove
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? (car los) s)
            (remove s (cdr los))
            (cons (car los) (remove s (cdr los)))))))

; test
(define lst_t2 '(a b c b s a e))
(remove 'b lst_t2)
