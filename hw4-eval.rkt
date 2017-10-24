#lang racket
(require racket/trace)
(require "program.rkt")
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eval
;; input: program-expression, environment as list of pairs (variable value)
;; output: '(Cannot Evaluate) or a number


(define (eval expr env)
  (if (caneval env)      ;; the environment might end up with a pair (variable '(Cannot Evaluate))
      (cond
        [ (number? expr) expr ]  ;; number
        [ (symbol? expr) (findvalue expr env) ] ;; sigma(x)

        ;; implementation of the semantics of variable expression
        [ (equal? (car expr) 'var) (evalvarassign (cadr expr) (cadr (cdr expr)) env) ]

        ;; implementation of the semantics of FExpr
        [ (equal? (car expr) 'fun) (eval (cadr (cdr expr)) (cons (cadr expr) env)) ]

        ;; implementation of the semantics of Apply
        ;; utter disregard for efficiency - searching the same environment three times using the same key
        ;; But this keeps it clean
        ;; We are going to translate the apply semantics to varexpression semantics
        ;; [[(apply (f (ArgList)))]]_env = [[(var Param-ArgValue list  FDef]]_staticenv
        [ (equal? (car expr) 'apply) (eval (list 'var
                                                 (paramargs (findfunparams (car (cadr expr))  ;; function name
                                                                           (length (cadr (cadr expr))) ;; number of params
                                                                           env)              ;; current environment
                                                            ;; findfunparams returns parameters of the function
                                                            (cadr (cadr expr)) ;; expressions representing arguments
                                                            env)
                                                 ;; paramargs returns the list of variable-value pairs
                                                 
                                                 (findfundef (car (cadr expr)) (length (cadr (cadr expr))) env)) ;; definition of the function
                                           
                                           (staticenv (car (cadr expr)) (length (cadr (cadr expr))) env) ;; static environment
                                           ) ]
 
        ;; same as before with the arithmatic operations: environment is added
        [ (arithop (car expr)) (evalarith (car expr) (cadr expr) (cadr (cdr expr)) env) ]

        ;; ifthenelse function
        [ else  (ifthenelse (evalcond (car expr) env) 
                            (cadr expr)
                            (cadr (cdr expr)) env) ]
        )
      '(Cannot Evaluate)
      ))


;; input: variable, environment
;; output: value to which the variable is mapped to in the environment
;;         It can be '(Cannot Evaluate) 
(define (findvalue x env)
  (if (null? env)  
      '(Cannot Evaluate)
      (if (equal? (car (car env)) x)
          (cadr (car env))
          (findvalue x (cdr env)))))

;; input: environment
;; output: true, if no variable is mapped to '(Cannot Evaluate)
;;         false, otherwise
;; Exercise: implement in another way, where it does not depend on '(Cannot Evaluate)
(define (caneval env)
  (if (null? env)
      true
      (and (not (equal? (cadr (car env)) '(Cannot Evaluate)))
           (caneval (cdr env)))))

;; input: list of (variable expression), expr to evaluate, environment
;; output: evaluate expr in some environment

(define (evalvarassign varassigns expr env)
  (if (null? varassigns)  ;; no variable expression pair, 
      (eval expr env)     ;; then just evaluate the expression in the current environment
      ;; else
      ;; recursive call with the suffix of varassigns, with the same expr
      ;; in the environment constructed by cons-ing (variable evaluation of expression)
      ;; to the given environment env.
      (evalvarassign (cdr varassigns)
                     expr
                     (cons (list (car (car varassigns))
                                 (eval (cadr (car varassigns)) env))
                           env))))

;; is op arithmatic operation
(define (arithop op)
  (or (equal? op '+)
      (equal? op '-)
      (equal? op '*)
      (equal? op '\))))

;; input: arithoperator, expr-operand1, expr-operand2, env
;; output: '(Cannot Evaluate) or some number
;; used: myapply 
(define (evalarith op expr1 expr2 env)
 (myapply op (eval expr1 env) (eval  expr2 env)))

;; input: true/false, '(Cannot Evaluate) expression values
;; output: '(Cannot Evaluate) or expression values
;;         expression values can be '(Cannot Evaluate)
(define (ifthenelse condition expr1 expr2 env)
  (if (equal? condition '(Cannot Evaluate))
      '(Cannot Evaluate)
      (if condition
          (eval expr1 env)
          (eval expr2 env))))

;; input: conditions of the form (gt/lt/eq expr1 expr2), (or/and cond1 cond2), (not cond)
;; output: true/false, '(Cannot Evaluate)
;; used: myapply
(define (evalcond condexpr env)
  (cond
    [ (equal? (car condexpr) 'gt)
      (myapply 'gt (eval (cadr condexpr) env) (eval (cadr (cdr condexpr)) env)) ]
    
    [ (equal? (car condexpr) 'lt)
      (myapply 'lt (eval (cadr condexpr) env) (eval (cadr (cdr condexpr)) env)) ]

    [ (equal? (car condexpr) 'eq)
      (myapply 'eq (eval (cadr condexpr) env) (eval (cadr (cdr condexpr)) env)) ]
    
    [ (equal? (car condexpr) 'and)
      (myapply 'and (evalcond (cadr condexpr) env)
               (evalcond (cadr (cdr condexpr)) env)) ]

    [ (equal? (car condexpr) 'or)
      (myapply 'or (evalcond (cadr condexpr) env)
               (evalcond (cadr (cdr condexpr)) env)) ]

    [ (equal? (car condexpr) 'not)
      (myapply 'not (evalcond (cadr condexpr) env)
               false) ] ;; dummy
    ))


;; input: some operator, arithmatic or conditional
;;        operand-values for the operator
;; output: '(Cannot Evaluate) or number or boolean 
(define (myapply op val1 val2)
  (if (or (equal? val1 '(Cannot Evaluate))
          (equal? val2 '(Cannot Evaluate)))
      '(Cannot Evaluate)
      (cond
        [ (equal? op '+) (+ val1 val2) ]
        [ (equal? op '-) (- val1 val2) ]
        [ (equal? op '*) (* val1 val2) ]
        [ (equal? op 'gt) (> val1 val2) ]
        [ (equal? op 'lt) (< val1 val2) ]
        [ (equal? op 'eq) (equal? val1 val2) ]
        [ (equal? op 'and) (and val1 val2) ]
        [ (equal? op 'or) (or val1 val2) ]
        [ (equal? op 'not) (not val1) ])))

;; Functions added for the assignment 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (paramargs (x1 x2 .... xn) (e1 e2 .... en) env) = ((x1 e1val) (x2 e2val) ... (xn enval))
;; input: list of variables
;;        list of expressions
;;        environment
;; output: list of pairs of variable-expressionvalue
(define (paramargs paramlist exprlist env)
  (if (null? paramlist)
      '()
      (cons (list (car paramlist) (eval (car exprlist) env))
            (paramargs (cdr paramlist) (cdr exprlist) env))))

;; find the function parameters
;; input: function name and arg-length
;;        env
;; output: list of function parameters
(define (findfunparams fname paramlength env)
  (if (and (list? (car (car env)))   ;; is a function definition
           (equal? (car (car (car env))) fname)  ;; name matches
           (equal? (length (cadr (car (car env)))) paramlength)) ;; paramlength matchs
      ;; 
      (cadr (car (car env)))   ;; return the list of parameters
      (findfunparams fname paramlength (cdr env)))) ;; else keep looking

;; Same as above: just return the definition of the function
(define (findfundef fname paramlength env)
  (if (and (list? (car (car env)))
           (equal? (car (car (car env))) fname)
           (equal? (length (cadr (car (car env)))) paramlength))
      ;; 
      (cadr (car env)) ;; return the definition of the function
      (findfundef fname paramlength (cdr env)))) ;; else keep looking

;; Given an environment; generate the static environment corresponding
;; for a function
;; same as above again
(define (staticenv fname paramlength env)
  (if (and (list? (car (car env)))
           (equal? (car (car (car env))) fname)
           (equal? (length (cadr (car (car env)))) paramlength))
      env ;; return the environment at the time of definition
      (staticenv fname paramlength (cdr env)))) ;; else keep looking