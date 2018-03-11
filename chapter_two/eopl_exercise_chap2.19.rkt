#lang eopl
; number->bintree : Int -> Bintree
(define number->bintree
  (lambda (n)
    (list n empty empty)))
; current-element : Bintree -> Number
(define current-element
  (lambda (bintree)
    (if (null? bintree) '()
        (car bintree))))
; move-to-left-son : Bintree -> Bintree
(define move-to-left-son
  (lambda(bintree)
    (if (null? bintree) (report-empty-bintree bintree)
        (cadr bintree))))
(define report-empty-bintree
  (lambda (bintree)
    (eopl:error "Bintree ~s is a empty list")))
; move-to-right-son : Bintree -> Bintree
(define move-to-right-son
  (lambda(bintree)
    (if (null? bintree)
        (report-empty-bintree bintree)
        (caddr bintree))))
; at-leaf? : Bintree -> Bool
(define at-leaf?
  (lambda (bintree)
    (if (null? bintree) #t #f)))
; insert-to-left : Int * Bintree -> Bintree
(define insert-to-left
  (lambda (n bintree)
    (list (current-element bintree)
          (list n (move-to-left-son bintree) empty)
          (move-to-right-son bintree))))
; insert-to-right : Int * Bintree -> Bintree
(define insert-to-right
  (lambda (n bintree)
    (list (current-element bintree)
          (move-to-left-son bintree)
          (list n empty (move-to-right-son bintree)))))
(define t1 (insert-to-right 14
                            (insert-to-left 12
                                            (number->bintree 13))))