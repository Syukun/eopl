#lang eopl
;number->sequence : Int -> Sequence
;(number->sequence 7) "should be" '(7 () ())
(define number->sequence
  (lambda(n)
    (list n empty empty)))
;current-element : Sequence -> Int
;(current-element '(6 (5 4 3 2 1) (7 8 9))) "should be" 6
(define current-element
  (lambda (seq)
    (car seq)))
;move-to-left : Sequence -> Sequence
;(move-to-left '(6 (5 4 3 2 1) (7 8 9)))
;"should be" '(5 (4 3 2 1) (6 7 8 9))
(define move-to-left
  (lambda (seq)
    (let[(left-list (cadr seq))
         (right-list (caddr seq))]
      (if (null? left-list) (report-no-left-number seq)
          (list (car left-list) (cdr left-list)
                (cons (current-element seq) right-list))))))
(define report-no-left-number
  (lambda (seq)
    (eopl:error 'move-to-left
                "Argument is at the left end of the sequence ~s" seq)))
;move-to-right : Seq -> Seq
;(move-to-right '(6 (5 4 3 2 1) (7 8 9))) "should be" '(7 (6 5 4 3 2 1) (8 9))
(define move-to-right
  (lambda(seq)
    (let[(left-list (cadr seq))
         (right-list (caddr seq))]
      (if (null? right-list) (report-no-right-number seq)
          (list (car right-list) (cons (current-element seq) left-list)
                (cdr right-list))))))
(define report-no-right-number
  (lambda(seq)
    (eopl:error 'move-to-right
             "Argument is at the right end of the sequence ~s" seq)))

;insert-to-left : Int * Seq -> Seq
;(insert-to-left 13 '(6 (5 4 3 2 1) (7 8 9))) "should be" '(6 (13 5 4 3 2 1) (7 8 9))
(define insert-to-left
  (lambda(n seq)
    (let[(left-list (cadr seq))
         (right-list (caddr seq))]
      (list (current-element seq)
            (cons n left-list)
            right-list))))
;insert-to-right : Int * Seq -> Seq
;(insert-to-right 13 '(6 (5 4 3 2 1) (7 8 9))) "should be" '(6 (5 4 3 2 1) (13 7 8 9))
(define insert-to-right
  (lambda(n seq)
    (let[(left-list (cadr seq))
         (right-list (caddr seq))]
      (list (current-element seq)
            left-list
            (cons n right-list)))))
; at-left-end? : n * Seq -> Bool
(define at-left-end?
  (lambda (n seq)
    (let[(left-list (cadr seq))]
      (cond [(null? left-list) #f]
            [(null? (cdr left-list))
             (if (eqv? (car left-list) n) #t #f)]
            [else (at-left-end? n (move-to-left seq))]))))
; at-right-end? : n * Seq -> Bool
(define at-right-end?
  (lambda (n seq)
    (let[(right-list (caddr seq))]
      (cond [(null? right-list) #f]
            [(null? (cdr right-list))
             (if (eqv? (car right-list) n) #t #f)]
            [else (at-right-end? n (move-to-right seq))]))))