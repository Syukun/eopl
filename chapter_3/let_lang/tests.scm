(module tests mzscheme
  
  (provide test-list)

  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
  
  (define test-list
    '(
  
      ;; simple arithmetic
      (positive-const "11" 11)
      (negative-const "-33" -33)
      (simple-arith-1 "-(44,33)" 11)
  
      ;; nested arithmetic
      (nested-arith-left "-(-(44,33),22)" -11)
      (nested-arith-right "-(55, -(22,11))" 44)
  
      ;; simple variables
      (test-var-1 "x" 10)
      (test-var-2 "-(x,1)" 9)
      (test-var-3 "-(1,x)" -9)
      
      ;; simple unbound variables
      (test-unbound-var-1 "foo" error)
      (test-unbound-var-2 "-(x,foo)" error)
  
      ;; simple conditionals
      (if-true "if zero?(0) then 3 else 4" 3)
      (if-false "if zero?(1) then 3 else 4" 4)
      
      ;; test dynamic typechecking
      (no-bool-to-diff-1 "-(zero?(0),1)" error)
      (no-bool-to-diff-2 "-(1,zero?(0))" error)
      (no-int-to-if "if 1 then 2 else 3" error)

      ;; make sure that the test and both arms get evaluated
      ;; properly. 
      (if-eval-test-true "if zero?(-(11,11)) then 3 else 4" 3)
      (if-eval-test-false "if zero?(-(11, 12)) then 3 else 4" 4)
      
      ;; and make sure the other arm doesn't get evaluated.
      (if-eval-test-true-2 "if zero?(-(11, 11)) then 3 else foo" 3)
      (if-eval-test-false-2 "if zero?(-(11,12)) then foo else 4" 4)

      ;; simple let
      (simple-let-1 "let x = 3 in x" 3)

      ;; make sure the body and rhs get evaluated
      (eval-let-body "let x = 3 in -(x,1)" 2)
      (eval-let-rhs "let x = -(4,1) in -(x,1)" 2)

      ;; check nested let and shadowing
      (simple-nested-let "let x = 3 in let y = 4 in -(x,y)" -1)
      (check-shadowing-in-body "let x = 3 in let x = 4 in x" 4)
      (check-shadowing-in-rhs "let x = 3 in let x = -(x,1) in x" 2)

      ;; test of Exercise 3.6
      (check-operator-minus "minus(-(minus(5),9))" 14)

      ;; test for Exercise 3.7
      (check-operator-addition "+(3,5)" 8)
      (check-operator-multiplication "*(3,5)" 15)
      (check-operator-quotient "/(5,3)" 1)

      ;; test for Exercise 3.8
      (check-operator-equal? "equal?(+(3,5),8)" #t)
      (check-operator-equal? "equal?(+(3,5),9)" #f)
      (check-operator-greater? "greater?(+(4,5),8)" #t)
      (check-operator-greater? "greater?(+(3,5),8)" #f)
      (check-operator-less? "less?(+(2,5),8)" #t)
      (check-operator-less? "less?(+(3,5),8)" #f)

      ;; test for Exercise 3.9
      ;(check-empty-list "emptylist" empty) ?? why not work??
      (check-car "car(cons(1,emptylist))" 1)
      (check-null?-and-cdr "null?(cdr(cons(1,emptylist)))" #t)
      ;(check-exapmle-on-textbook "let x = 4 in cons(x,cons(cons(-(x,1),emptylist),emptylist))" (cons 4 (cons 3 empty)))

      ;; test for Exercise 3.10
      ;(check-list-exp "let x = 4 in list(x,-(x,1),-(x,3))" '(4 3 1))
      ))
  )