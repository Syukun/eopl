(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the LET language.  The \commentboxes are the
  ;; latex code for inserting the rules into the code in the book.
  ;; These are too complicated to put here, see the text, sorry.

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 71
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 71
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))

        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env))))
        
        ;Exercise 3.6
        ;\commentbox{\theminusspec}
        (minus-exp (exp)
            (let [(exp-val (value-of exp env))]
              (num-val (- (expval->num exp-val)))))

        ;Exrecise 3.7
        ;\commentbox{\theaddspec}
        (add-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (+ num1 num2)))))
        ;\commentbox{\theaddspec}
        (mul-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (* num1 num2)))))
        ;\commentbox{\theaddspec}
        (quo-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (quotient num1 num2)))))

        ;Exercise 3.8
        (equal?-exp (exp1 exp2)
           (let [(val1 (value-of exp1 env))
                 (val2 (value-of exp2 env))]
             (if (eq? (expval->num val1)
                      (expval->num val2))
                 (bool-val #t)
                 (bool-val #f))))
        (greater?-exp (exp1 exp2)
           (let [(val1 (value-of exp1 env))
                 (val2 (value-of exp2 env))]
             (if (> (expval->num val1)
                      (expval->num val2))
                 (bool-val #t)
                 (bool-val #f))))
        (less?-exp (exp1 exp2)
           (let [(val1 (value-of exp1 env))
                 (val2 (value-of exp2 env))]
             (if (< (expval->num val1)
                      (expval->num val2))
                 (bool-val #t)
                 (bool-val #f))))

        ;Exercise 3.9
        ;emptylist
        (empty-list-exp ()
           (empty-list-val))
        ;cons
        (cons-exp (exp1 exp2)
           (let [(val1 (value-of exp1 env))
                 (val2 (value-of exp2 env))]
             (cons-val val1 val2)))
        ;car
        (car-exp (exp)
           (let [(val (value-of exp env))]
             (cases expval val
               (cons-val (car-val cdr-val)
                         car-val)
               (else (eopl:error 'car-exp
                                 "Looking for a list expression, ~s is not one of list" exp)))))
        ;cdr
        (cdr-exp (exp)
           (let [(val (value-of exp env))]
             (cases expval val
               (cons-val (car-val cdr-val)
                         cdr-val)
               (else (eopl:error 'car-exp
                                 "Looking for a list expression, ~s is not one of list" exp)))))

        ;null?
        (null?-exp (exp)
           (let [(val (value-of exp env))]
             (cases expval val
               (cons-val (car-val cdr-val) (bool-val #f))
               (empty-list-val () (bool-val #t))
               (else (eopl:error 'null?-exp "This is not a list")))))

        ; Exercise 3.10
        (list-exp (exps)
           (let [(vals (value-of-exps exps env))]
             (vals->expval vals)))      
        ))

      )
  ;; value-of-exps : Expressions * Env -> ExpVals
  (define value-of-exps
    (lambda (exps env)
      (map (lambda (exp) (value-of exp env))
           exps)))
  ;; vals->expval : ListOf(ExpVal) -> ExpVal
  (define vals->expval
    (lambda (vals)
      (cond
        [(null? vals) (empty-list-val)]
        [(pair? vals) (cons-val (car vals)
                                (vals->expval (cdr vals)))]
        [else (eopl:error 'vals->expval
                          "Need a list , ~s is not one" vals)])))

  )

