(module lang

  ;; grammar for the LET language

  (lib "eopl.ss" "eopl")                
  
  (require "drscheme-init.scm")
  
  (provide (all-defined-out))

  ;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;
  
  (define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier
       (letter (arbno (or letter digit "_" "-" "?")))
       symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))
  
  (define the-grammar
    '((program (expression) a-program)

      (expression (number) const-exp)
      (expression
        ("-" "(" expression "," expression ")")
        diff-exp)
      
      (expression
       ("zero?" "(" expression ")")
       zero?-exp)

      (expression
       ("if" expression "then" expression "else" expression)
       if-exp)

      (expression (identifier) var-exp)

      (expression
       ("let" identifier "=" expression "in" expression)
       let-exp)
      
      ;; Exercise 3.6
      ;; Extend the language by adding a new operator "minus"
      (expression
       ("minus" "(" expression ")")
       minus-exp)

      ;; Exercise 3.7
      ;; Extend the language by adding operators for addition , multiplication ,and integer quotient
      ;; addition
      (expression
       ("+" "(" expression "," expression ")")
       add-exp)
      ;; multiplication
      (expression
       ("*" "(" expression "," expression ")")
       mul-exp)
      ;; integer quotient
      (expression
       ("/" "(" expression "," expression ")")
       quo-exp)

      ;; Exercise 3.8
      ;; add a numeric equality predicate equal?
      (expression
       ("equal?" "(" expression "," expression ")")
       equal?-exp)
      ;; add a numeric order predicates greater?
      (expression
       ("greater?" "(" expression "," expression ")")
       greater?-exp)
      ;; add a numeric order predicates less?
      (expression
       ("less?" "(" expression "," expression ")")
       less?-exp)

      ;; Exercise 3.9
      ;; ExpVal = Int | Bool | List
      ;; DenVal = Int | Bool | List
      ;; empty-list-exp
      (expression
       ("emptylist")
       empty-list-exp)
      ;; cons-exp
      (expression
       ("cons" "(" expression "," expression ")" )
       cons-exp)
      ;; car-exp
      (expression
       ("car" "(" expression ")")
       car-exp)
      ;; cdr-exp
      (expression
       ("cdr" "(" expression ")")
       cdr-exp)
      ;; null?-exp
      (expression
       ("null?" "(" expression ")")
       null?-exp)

      ;; Exercise 3.10
      ;; list-exp
      (expression
       ("list" "(" (separated-list expression ",") ")")
       list-exp)
      ))

  
  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
  
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))
  
  )
