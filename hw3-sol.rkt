#lang racket
(require "program.rkt")
(provide (all-defined-out))


(define (synchk expr)
  (cond
    [ (number? expr) true ]  ;; number
    
    [ (symbol? expr) true ]  ;; variable
    
    [ (and (equal? (car expr) 'var)   ;; first element is var
           (equal? 3 (length expr))) ;; there are three elements
      ;; then
      (and (> (length (cadr expr)) 0) ;; there must be at least one varassign
           (synchkvarassignseq (cadr expr)) ;; lets check the varassignseq
           (synchk (cadr (cdr expr)))) ]    ;; finally the expression at the end

    [ (and (arithop (car expr))        ;; first element is +, -, /, *
           (equal? 3 (length expr))) ;; there are three elements
      ;; then
      (and (synchk (cadr expr))
           (synchk (cadr (cdr expr)))) ]

    [ (and (list? (car expr))        ;; first element is a list
           (equal? 3 (length expr))) ;; there are three elements
      ;; then
      (and (synchkcond (car expr))   ;; it is a condition
           (synchk (cadr expr))      ;; validate the then expression
           (synchk (cadr (cdr expr)))) ] ;; validate the else expression
    
    [ (and (equal? (car expr) 'fun)   ;; first element is fun
           (equal? 3 (length expr))) ;; there are three elements
      ;; then
      (and (> (length (cadr expr)) 0) ;; check for FExpr
           (synchkfassignseq (cadr expr)) ;; check the FAssignSeq
           (synchk (cadr (cdr expr)))) ]    ;; finally the expression at the end
    
    [ (and (equal? (car expr) 'apply)   ;; first element is apply
           (equal? 2 (length expr))) ;; there are two elements
      ;; then
      (and (equal? (length (cadr expr)) 2) ;; check for FName and Args - must have both
           (symbol? (cadr expr)) ;; check the Fname
           (args (cadr (cdr expr)))) ]    ;; finally the args at the end
    
    [ else false ]))

(define (synchkvarassignseq listofpairs)
  (if (null? listofpairs)
      true
      ;; else
      (and (equal? (length (car listofpairs)) 2)  ;; must be a pair
           (symbol? (car (car listofpairs)))      ;; first element in pair is variable
           (synchk (cadr (car listofpairs)))      ;; second element is an expression
           (synchkvarassignseq (cdr listofpairs)))))

(define (synchkfassignseq listofpairs)
  (if (null? listofpairs)
      true
      ;; else
      (and (equal? (length (car listofpairs)) 2)  ;; must be a pair
           (symbol? (car (car listofpairs)))      ;; first element in first pair is symbol
           (formalparams (cadr (car listofpairs)))      ;; second element is FormalParams
           (synchk (cdr listofpairs)))))

(define (args list)
  (if (empty? list)
      true
      ;; else
      (and (synchk (car (list)))  ;; must be an expr
           (args (cdr list)))))

(define (formalparams params)
  (if (null? params)
      true
      ;; else
      (and (symbol? (car params))      ;; first element in pair is symbol
           (formalparams (cdr params)))      ;; second element is FormalParams
))

(define (synchkcond condition)
  (cond
    [ (and (or (equal? 'gt (car condition))
               (equal? 'lt (car condition))
               (equal? 'eq (car condition)))
           (equal? 3 (length condition)))
      (and (synchk (cadr condition))
           (synchk (cadr (cdr condition)))) ]
    [ (and (or (equal? 'and (car condition))
               (equal? 'or (car condition)))
           (equal? 3 (length condition)))
      (and (synchkcond (cadr condition))
           (synchkcond (cadr (cdr condition)))) ]
    [ (and (equal? 'not (car condition))
           (equal? 2 (length condition)))
      (synchkcond (cadr condition)) ]
    [ else false] ))


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
 
        ;; same as before with the arithmatic operations: environment is added
        [ (arithop (car expr)) (evalarith (car expr) (cadr expr) (cadr (cdr expr)) env) ]

        ;; ifthenelse function
        [ else  (ifthenelse (evalcond (car expr) env) 
                            (eval (cadr expr) env)
                            (eval (cadr (cdr expr)) env)) ]
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
(define (ifthenelse condition expr1 expr2)
  (if (equal? condition '(Cannot Evaluate))
      '(Cannot Evaluate)
      (if condition
          expr1
          expr2)))

;; input: conditions of the form (gt/lt/eq expr1 expr2), (or/and cond1 cond2), (not cond)
;; output: true/false, '(Cannot Evaluate)
;; used: myapply
(define (evalcond condexpr env)
  (cond
    [ (equal? (car condexpr) 'gt)
      (myapply 'gt (eval (cadr condexpr) env) (eval (cadr (cdr condexpr)) env)) ]
    
    [ (equal? (car condexpr) 'lt)
      (myapply 'lt (eval (cadr condexpr) env) (eval (cadr (cdr condexpr)) env)) ]
    
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