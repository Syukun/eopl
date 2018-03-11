; Exercise 1.31
(define leaf
  (lambda (i) i))
(define interior-node
  (lambda (s i1 i2)
    (list s i1 i2)))
; leaf? : Bintree -> Booleam
; usage : too test wheter a bintree is a leaf
; Example : (leaf? '(foo 1 2)) is #f
;           (leaf? 2) is #t
(define leaf?
  (lambda (bintree)
    (number? bintree)))
; lson : Bintree -> Bintree
; usage : return left part of a  bintree
; Example : (lson '(foo 1 2)) "should be" 1
(define lson
  (lambda (bintree)
    (if (number? bintree) (report-no-left-part bintree)
        (cadr bintree))))
(define report-no-left-part
  (lambda (bintree)
    (eopl:error 'lson
                "Bintree ~s  does have left part. ~%" bintree)))
; rson : Bintree -> Bintree
; usage : return right part of a bintree
; Example : (rson '(foo 1 2)) "should be" 2
(define rson
  (lambda (bintree)
    (if (number? bintree) (report-no-right-part bintree)
        (caddr bintree))))
(define report-no-right-part
  (lambda (bintree)
    (eopl:error 'lson
                "Bintree ~s  does have right part. ~%" bintree)))
; contents-of : Bintree -> Int or Symbol
; usage : return contents of Bintree whether it is a leave or interior node
; Example : (contents-of '(foo 1 2)) "should be" foo
;           (contents-of 2) "should be" 2
(define contents-of
  (lambda (bintree)
    (if (leaf? bintree) bintree
        (car bintree))))

; Exercise 1.32
; double-tree : Bintree -> Bintree
; usage : return a bintree which all the integer in the original bintree doubled
; Example (double-tree '(foo 1 2)) "should be" '(foo 2 4)
(define double-tree
  (lambda (bintree)
    (if (leaf? bintree) (* 2 bintree)
        (cons (car bintree)
              (cons (double-tree (lson bintree))
                    (cons (double-tree (rson bintree)) empty))))))

; Exercise 1.33
; mark-leaves-with-red-depth : Bintree -> Bintree
; usage : return a new bintree which each leaf contains the integer of nodes
;         between it and the root that contain the symbol red.
; Example :
;(mark-leaves-with-red-depth
; (interior-node 'red
;                (interior-node 'bar
;                               (leaf 26)
;                               (leaf 12))
;                (interior-node 'red
;                               (leaf 11)
;                               (interior-node 'quux
;                                              (leaf 117)
;                                              (leaf 14)))))
;should be
;(red
;    (bar 1 1)
;    (red 2 (quux 2 2)))
(define mark-leaves-with-red-depth
	(lambda (bintree)
		(mark-leaves-with-red-accumulated bintree 0)))

(define mark-leaves-with-red-accumulated
	(lambda (bintree n)
		(cond ((leaf? bintree) n)
			((eq? (contents-of bintree) 'red)
				(interior-node 'red
					(mark-leaves-with-red-accumulated (lson bintree) (+ n 1))
					(mark-leaves-with-red-accumulated (rson bintree) (+ n 1))))
			(else
				(interior-node (contents-of bintree)
					(mark-leaves-with-red-accumulated (lson bintree) n)
					(mark-leaves-with-red-accumulated (rson bintree) n))))))

; Exercise 1.34
; path : Int * binary-search-tree-> list-of-directions(left | right)
; usage : returns a list of lefts and rights showing how to find the node containing n.
; Example : (path 17 '(14 (7 () (12 () ()))
;                       (26 (20 (17 () ()) ())
 ;                          (31 () ()))))
;"should be" (right left left)
(define path
  (lambda (n bst)
    (if (null? bst) (report-n-in-bst n bst)
        (cond [(< n (car bst)) (cons 'left (path n (cadr bst)))]
              [(> n (car bst)) (cons 'right (path n (caddr bst)))]
              [else empty]))))
(define report-n-in-bst
  (lambda (n bst)
    (eopl:error 'path
                "~s don't have ~s" bst n)))
            
; Exercise 1.35
; number-leaves : Bintree -> Bintree
; usage : return a bintree which the contents of the leaves are numbered from 0
; Exapmle : (number-leaves (interior-node 'foo (interior-node 'bar (leaf 26) (leaf 12))
;             (interior-node 'baz (leaf 11) (interior-node 'quux (leaf 117) (leaf 14))
;           "should be"
;  (foo
;    (bar 0 1)
;    (baz
;     2
;     (quux 3 4)))
(define number-leaves
  (lambda (bintree)
    (number-leaves-auxi bintree 0)))
; number-leaves-auxi : Bintree * Int -> Bintree
; usage : return a final-outcome-kind bintree which left begin from n
(define number-leaves-auxi
  (lambda (bintree n)
    (if (leaf? bintree) n
        (list (contents-of bintree)
              (number-leaves-auxi (lson bintree) n)
              (number-leaves-auxi (rson bintree) ( + n (leaves-number (lson bintree))))))))
(define leaves-number
  (lambda (bintree)
    (if (leaf? bintree) 1
        (+ (leaves-number (lson bintree))
           (leaves-number (rson bintree))))))

; Exercise 1.36
; number-elements : List -> Listof(List(Int,SchemeVal))
; usage : satisfied same function in page 23.
(define number-elements
  (lambda (lst)
    (if (null? lst) '()
        (g (list 0 (car lst)) (number-elements (cdr lst))))))
(define g
  (lambda (ele loele)
    (cons ele (add-one loele))))
(define add-one
  (lambda (loele)
    (if (null? loele) '()
        (cons (add-one-ele (car loele))
              (add-one (cdr loele))))))
(define add-one-ele
  (lambda (ele)
    (cons (+ 1 (car ele)) (cdr ele))))
