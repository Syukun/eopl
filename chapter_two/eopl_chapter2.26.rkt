#lang eopl
; Exercise 2.26
(define-datatype
  red-blue-tree red-blue-tree?
  (r-b-subtree (red-blue-subtree red-blue-subtree?)))
(define-datatype
  red-blue-subtree red-blue-subtree?
  (red-node (left-red-blue red-blue-subtree?) (right-red-blue red-blue-subtree?))
  (blue-node (red-blue-lists (list-of red-blue-subtree?)))
  (leaf-node (i number?)))
; trans : Red-blue-tree -> Red-blue-tree
; usage : return a tree of the same shape, except that each leaf node is replaced by a leaf
;         node that contains the number of red nodes on the path between it and the root.
(define trans
  (lambda (rbt)
    (cases red-blue-tree rbt
      (r-b-subtree (r-b-t) (trans-subtree r-b-t))
      (else "Wrong input in function trans"))))
(define trans-subtree
  (lambda (rbst)
    (cases red-blue-subtree rbst
      (red-node (left-red-blue right-red-blue)
                (red-node (trans-subtree (addOne left-red-blue))
                          (trans-subtree (addOne right-red-blue))))
      (blue-node (red-blue-lists)
                 (blue-node (map trans-subtree red-blue-lists)))
      (leaf-node (i) (leaf-node i)))))
; addOne : Red-blue-subtree -> Red-blue-subtree
(define addOne
  (lambda (rbst)
    (cases red-blue-subtree rbst
      (red-node (left-red-blue right-red-blue)
                (red-node (addOne left-red-blue)
                          (addOne right-red-blue)))
      (blue-node (red-blue-lists)
                 (blue-node (map addOne red-blue-lists)))
      (leaf-node (i) (leaf-node (+ i　1))))))

; Test
(define rbt1 (r-b-subtree (leaf-node 0)))
(define rbt2 (r-b-subtree (red-node (leaf-node 0) (leaf-node 0))))
(define rbt3 (r-b-subtree (blue-node (list (red-node (leaf-node 0) (blue-node (list (leaf-node 0))))
                                     (leaf-node 0)))))