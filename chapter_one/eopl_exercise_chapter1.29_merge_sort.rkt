#lang eopl
;; merge : ListOf(Int) *  ListOf(Int) -> ListOf(Int)
(define merge
  (lambda (loi1 loi2)
    (cond [(and (null? loi1) (null? loi2)) empty]
          [(null? loi1) loi2]
          [(null? loi2) loi1]
          [else
           (if(<= (car loi1) (car loi2))
              (cons (car loi1) (merge (cdr loi1) loi2))
              (cons (car loi2) (merge loi1 (cdr loi2))))])))
; merge-sort : ListOf(Int) -> ListOf(Int)
(define merge-sort
  (lambda (loi)
    (cond
      [(null? loi) '()]
      [(null? (cdr loi)) loi]
      [else
       (merge (merge-sort (left-part loi)) (merge-sort (right-part loi)))])))
(define left-part
  (lambda (loi)
    (left-part-auxi loi (quotient (+ 1(length loi)) 2))))
; (left-part-auxi '(2 3 5) 2) "should be" '(2 3)
(define left-part-auxi
  (lambda (loi n)
    (if (eqv? n 1) (cons (car loi) empty)
        (cons (car loi) (left-part-auxi (cdr loi) (- n 1))))))

(define right-part
  (lambda (loi)
    (right-part-auxi loi (quotient (+ 1(length loi)) 2))))
(define right-part-auxi
  (lambda (loi n)
    (if (zero? n) loi
        (right-part-auxi (cdr loi) (- n 1)))))

(define loi_t '(49 38 65 97 76 13 27 49))