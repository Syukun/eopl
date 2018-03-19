#lang eopl
; Exercise 2.21
(define-datatype env env?
  (empty-env)
  (extend-env
   (var symbol?)
   (val number?)
   (body-env env?)))
(define apply-env
  (lambda (given-env search-var)
    (cases env given-env
      (empty-env () (report-no-binding-found search-var))
      (extend-env (var val body-env)
                  (if(eqv? var search-var) val
                     (apply-env body-env search-var))))))
(define report-no-binding-found
  (lambda(search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))
(define has-binding?
  (lambda (given-env search-var)
    (cases env given-env
      (empty-env () #f)
      (extend-env (var val body-env)
                  (if (eqv? var search-var) #t
                      (has-binding? body-env search-var))))))

;; Problem : How to write test??

; Exercise 2.22
; Stack data type
(define-datatype stack stack?
  (empty-stack)
  (push-stack
   (val number?)
   (orig-stack stack?))
  (pop-stack
   (remain-stack stack?)))

(define empty-s
  (lambda ()
    (empty-stack)))
(define push
  (lambda (v s)
    (push-stack v s)))
(define pop
  (lambda (s)
    (cases stack s
      (empty-stack () "empty-stack")
      (push-stack (val orig-stack)
                  orig-stack)
      (pop-stack (remain-stack) remain-stack))))
(define top
  (lambda (s)
    (cases stack s
      (empty-stack () "empty-stack")
      (push-stack (val orig-stack) val)
      (pop-stack (remain-stack) (top remain-stack)))))
(define empty-stack?
  (lambda (s)
    (cases stack s
      (empty-stack () #t)
      (push-stack (val orig-stack) #f)
      (pop-stack (remain-stack) (empty-stack? remain-stack)))))

; Test
(define s_test (push 3 (push 2 (push 1 (empty-s)))))
(equal? (top s_test) 3)

; Exercise 2.23
(define-datatype lc-exp lc-exp?
  (var-exp (var var?))
  (lambda-exp (bound-var var?)
              (body lc-exp?))
  (app-exp (rator lc-exp?)
           (rand lc-exp?)))
; var? : Var -> Bool
; return #t if var is a symbol other than "lambda"
(define var?
  (lambda (var)
    (if(eq? var "lambda") #f #t)))


; Exercise 2.24
(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))
; bintree-to-list : Bintree -> List
; return list from bintree
(define bintree-to-list
  (lambda (btree)
    (cases bintree btree
      (leaf-node (num) (list 'leaf-node num))
      (interior-node
       (key left right)
       (list 'interior-node key
             (bintree-to-list left)
             (bintree-to-list right))))))
; test
(eq? (bintree-to-list (interior-node 'a (leaf-node 3) (leaf-node 4)))
     (interior-node 'a (leaf-node 3) (leaf-node 4)))

(define max-interior
  (lambda (btree)
    (cases bintree btree
      (leaf-node (num) (report-leaf-node btree))
      (interior-node
       (key left right)
       (let [(left-val (sum-val left))
             (right-val (sum-val right))]
         (cond [(<= left-val 0)
                (if (<= right-val 0)
                    (if (<= left-val right-val)
                        (max-interior right)
                        (max-interior left))
                    (max-interior right))]
               [(<= right-val 0) (max-interior left)]
               [else key]))))))
(define sum-val
  (lambda (btree)
    (cases bintree btree
      (leaf-node (num) num)
      (interior-node
       (key left right)
       (+ (sum-val left)
          (sum-val right))))))
(define report-leaf-node
  (lambda (btree)
    (eopl:error 'max-interior "Binary ~s tree must have at least one interior" btree)))
(define tree-1
  (interior-node 'foo (leaf-node 2) (leaf-node 3)))
(define tree-2
  (interior-node 'bar (leaf-node -1) tree-1))
(define tree-3
  (interior-node 'baz tree-2 (leaf-node 1)))
; Test
(eq? (max-interior tree-2) 'foo)
(eq? (max-interior tree-3) 'baz)
