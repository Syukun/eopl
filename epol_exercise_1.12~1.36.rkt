#lang eopl
; Exercise 1.13
(define subst-map
  (lambda (new old slist)
    (map (lambda (sexp) (subst-in-s-exp-map new old sexp)) slist)))
(define subst-in-s-exp-map
  (lambda (new old sexp)
    (if (symbol? sexp)
        (if (eqv? sexp old) new sexp)
        (subst-map new old sexp))))



; Exercise 1.15
; duple : Int * SchemeValue -> list-of-scheme-value
; usage : returns a list containing n copies of x
; example : (duple 2 3) "should be" (3 3)
; (duple 4 '(ha ha)) "should be" '((ha ha) (ha ha) (ha ha) (ha ha))

(define duple
  (lambda (n x)
    (if (zero? n)
        '()
        (cons x (duple (- n 1) x)))))
; Exercise 1.16
; invert : list-of-2-list -> list-of-2-list
; usage : return a list with each 2-list reversed
; example : (invert '((a 1) (a 2) (1 b) (2 b))) "should be" ((1 a) (2 a) (b 1) (b 2))
(define invert
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (reverse (car lst))
              (invert (cdr lst))))))
(define reverse
  (lambda (2-list)
    (cons (cadr 2-list)
          (cons (car 2-list) empty))))

; Exercise 1.17
; down : list-of-value -> list-of-(value)
; usage : wrap parentheses around each top-level element of lst
; example : (down '(1 2 3)) "should be" ((1) (2) (3))
; (down '((a) (fine) (idea))) "should be" (((a)) ((fine)) ((idea)))
; (down '(a (more (complicated)) object)) "should be" ((a) ((more (complicated))) (object))
(define down
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (cons (car lst) empty) (down (cdr lst))))))


; Exercise 1.18
; swapper : Sym * Sym * s-list -> s-list
; usage : returns a list the same as slist, but
; with all occurrences of s1 replaced by s2
; and all occurrences of s2 replaced by s1.
; Example : (swapper 'a 'd '(a b c d)) "should be" (d b c a)
; (swapper 'a 'd '(a d () c d)) "should be" (d a () c a)
; (swapper 'x 'y '((x) y (z (x)))) "should be" ((y) x (z (y)))
(define swapper
  (lambda (s1 s2 slist)
    (if (null? slist)
        '()
        (cons (swapper-s-exp s1 s2 (car slist))
              (swapper s1 s2 (cdr slist))))))
(define swapper-s-exp
  (lambda (s1 s2 s-exp)
    (if (symbol? s-exp)
        (cond
          ((eqv? s-exp s1) s2)
          ((eqv? s-exp s2) s1)
          (else s-exp))
        (swapper s1 s2 s-exp))))

; Exercise 1.19
; list-set : List * Int * SchemeVal -> List
; usage : returns a list like lst,except that the n-th element, using zero-based indexing, is x.
; Example : (list-set '(a b c d) 2 '(1 2)) "should be" (a b (1 2) d)
;           (list-ref (list-set '(a b c d) 3 '(1 5 10)) 3) "should be" (1 5 10)
(define list-set
  (lambda (lst n x)
    (if(null? lst)
       '()
       (if (zero? n)
           (cons x (cdr lst))
           (cons (car lst) (list-set (cdr lst) (- n 1) x))))))

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

; Exercise 1.21
; product : list-of-symbol * list-of-symbol -> list-of-2-lists
; usage : returns a list of 2-lists that represents the Cartesian product of sos1 and sos2. The 2-lists may appear in any order.
; Example : (product '(a b c) '(x y)) "should be" ((a x) (a y) (b x) (b y) (c x) (c y))
(define product
  (lambda (sos1 sos2)
    (if (null? sos1) '()
        (append (product-auxi (car sos1) sos2)
                (product (cdr sos1) sos2)))))
(define product-auxi
  (lambda (s sos)
    (if (null? sos) '()
        (cons (cons s (cons (car sos) empty))
              (product-auxi s (cdr sos))))))

; Exercise 1.22
; filter-in : Predicate * List -> List
; usage : returns the list of those elements in lst that satisfy the predicate pred.
; Example : (filter-in number? '(a 2 (1 3) b 7)) "should be" (2 7)
;           (filter-in symbol? '(a (b c) 17 foo)) "should be" (a foo)
(define filter-in
  (lambda (pred lst)
    (if (null? lst) '()
        (if (pred (car lst))
            (cons (car lst) (filter-in pred (cdr lst)))
            (filter-in pred (cdr lst))))))

; Exercise 1.23
; list-index : Predicate * List -> Int or #f
; usage : returns the 0-based position of the first element of lst that satisfies the predicate pred.
;         If no element of lst satisfies the predicate, then list-index returns #f.
; Example : (list-index number? '(a 2 (1 3) b 7)) "should be" 1
;           (list-index symbol? '(a (b c) 17 foo)) "should be" 0
;           (list-index symbol? '(1 2 (a b) 3)) "should be" #f
(define list-index
  (lambda (pred lst)
    (list-index-auxi pred lst 0)))
(define list-index-auxi
  (lambda (pred lst n)
    (if (null? lst) #f
        (if (pred (car lst)) n
            (list-index-auxi pred (cdr lst) (+ 1 n))))))

; Exercise 1.24
; every? : Predicate * List -> Boolean
; usage : returns #f if any element of lst fails to satisfy pred, and returns #t otherwise.
; Example : (every? number? '(a b c 3 e)) "should be" #f
;           (every? number? '(1 2 3 5 4)) "should be" #t
(define every?
  (lambda (pred lst)
    (if (null? lst) #t
        (if (pred (car lst)) (every? pred (cdr lst))
            #f))))

; Exercise 1.25
; exist? : Predicate * List -> Boolean
; usage : returns #t if any element of lst satisfies pred, and returns #f otherwise.
; Example : (exist? number? '(a b c 3 e)) "should be" #t
;           (exist? number? '(a b c d e)) "should be" #f
(define exist?
  (lambda (pred lst)
    (if (null? lst) #f
        (if (pred (car lst)) #t
            (exist? pred (cdr lst))))))

; Exercise 1.26
; up : List -> List
; usage : removes a pair of parentheses from each top-level element of lst.
;         If a top-level element is not a list, it is included in the result, as is.
;         The value of (up (down lst)) is equivalent to lst, but (down (up lst)) is not necessarily lst.
; Example : (up '((1 2) (3 4))) "should be" (1 2 3 4)
;           (up '((x (y)) z)) "should be" (x (y) z)
(define up
  (lambda (lst)
    (cond ((null? lst) '())
          ((list? (car lst)) (append (car lst) (up (cdr lst))))
          (else (cons (car lst) (up (cdr lst)))))))

; Exercise 1.27
; flatten : slist -> List
; usage :returns a list of the symbols contained in slist in the order in which they occur when slist is printed.
;        Intuitively, flatten removes all the inner parentheses from its argument.
; Example : (flatten '(a b c)) "should be" (a b c)
;           (flatten '((a) () (b ()) () (c))) "should be" (a b c)
;           (flatten '((a b) c (((d)) e))) "should be" (a b c d e)
;           (flatten '(a b (() (c)))) "should be" (a b c)
(define flatten
  (lambda (slist)
    (if (null? slist) '()
        (if(not (null? (flatten-s-exp (car slist))))
           (append (flatten-s-exp (car slist))
                   (flatten (cdr slist)))
           (flatten (cdr slist))))))
(define flatten-s-exp
  (lambda (s-exp)
    (if (symbol? s-exp) (cons s-exp empty)
        (flatten s-exp))))

; Exercise 1.28
; merge : list-of-int * list-of-int -> list-of-int
; usage : returns a sorted list of all the integers in loi1 and loi2.
;         where loi1 and loi2 are lists of integers that are sorted in ascending order
; Example : (merge '(1 4) '(1 2 8)) "should be" (1 1 2 4 8)
;           (merge '(35 62 81 90 91) '(3 83 85 90)) "should be" (3 35 62 81 83 85 90 90 91)
(define merge
  (lambda (loi1 loi2)
    (cond ((and (null? loi1) (null? loi2)) '())
          ((null? loi1) loi2)
          ((null? loi2) loi1)
          (else
           (if(< (car loi1) (car loi2))
              (cons (car loi1) (merge (cdr loi1) loi2))
              (cons (car loi2) (merge loi1 (cdr loi2))))))))

; Exercise 1.29
; sort : list-of-Int -> list-of-Int
; usage : returns a list of the elements of loi in ascending order.
; Example : (sort '(8 2 5 2 3)) "should be" (2 2 3 5 8)
(define sort
  (lambda (loi)
    (if (null? loi) '()
        (sort-auxi (car loi) (sort (cdr loi))))))
(define sort-auxi
  (lambda (i loi-sorted)
    (if(null? loi-sorted) (cons i loi-sorted)
       (if(< i (car loi-sorted)) (cons i loi-sorted)
          (cons (car loi-sorted) (sort-auxi i (cdr loi-sorted)))))))

; Exercise 1.30
; sort/predicate : Predicate * list-of-Int -> list-of-Int
; usage : returns a list of elements sorted by the predicate.
; Example : (sort/predicate < '(8 2 5 2 3)) "should be" (2 2 3 5 8)
;           (sort/predicate > '(8 2 5 2 3)) "should be" (8 5 3 2 2)
(define sort/predicate
  (lambda (pred loi)
    (if (null? loi) '()
        (sort-predicate-auxi pred (car loi) (sort/predicate pred (cdr loi))))))
(define sort-predicate-auxi
  (lambda (pred i loi-sorted)
    (if (null? loi-sorted) (cons i loi-sorted)
        (if(pred i (car loi-sorted)) (cons i loi-sorted)
           (cons (car loi-sorted) (sort-predicate-auxi pred i (cdr loi-sorted)))))))


; Definition 1.1.7 (binary tree)
; Bintree::=Int |(Symbol Bintree Bintree)

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
