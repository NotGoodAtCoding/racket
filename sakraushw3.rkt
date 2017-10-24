#lang racket
(require "program.rkt")
(require "tests.rkt")
(provide (all-defined-out)) ;; for us to test
(require racket/trace) ;; in case you want to use tracing

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;BEGIN GRAMMAR CHECK DEFINITONS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Returns true if the input program follows the given
;; semantics, false otherwise.
;; Input should be a program to test and an environment
;; Output is true or false
;; Ex: (synchk '(+ 1 2) '((x 2))) -> true
(define (synchk expr env)
  (cond
    [ (isexpr expr) true]
    [ else false ]
    ))

;; Returns true if the given expression is a valid Expr
;; by the rules of the grammar
;; This means that the input is:
;; A Number
;; A Symbol
;; An OpExpr
;; as defined. Returns false otherwise.
(define (isexpr expr)
  (cond
    [ (number? expr)  true ]
    [ (symbol? expr)  true ]
    [ (isopexpr expr)  true ]
    [ else false ]
    )
  )

;; Returns true if the given expression is a valid OpExpr
;; by the rules of the grammar
;; This means that the input is:
;; An ArithExpr
;; A CondExpr
;; A VarExpr
;; as defined. Returns false otherwise.
(define (isopexpr expr)
  (cond
    [ (isarithexpr expr) true]
    [ (iscondexpr expr) true]
    [ (isvarexpr expr) true]
    [else false]
    )
  )

;; Returns true if the given expression is a valid ArithExpr
;; by the rules of the grammar
;; This means that the input is:
;; (Op Expr Expr)
;; where Op : (+,-,*,/)
;; as defined. Returns false otherwise.
(define (isarithexpr expr)
 (cond
   [ (isarith (car expr))  (and (equal? (length expr) 3)
                                  (isexpr (cadr expr))
                                  (isexpr (cadr (cdr expr)))) true ]
   [ else false ]
   )
  )

;; Returns true if the given expression is a valid CondExpr
;; by the rules of the grammar
;; This means that the input is:
;; (CCond Expr Expr)
;; as defined. Returns false otherwise.
(define (iscondexpr expr)
  (cond
    [ (isccond expr) true]
    [ else false ]
    )
  )

;; Returns true if the given expression is a valid CCond
;; by the rules of the grammar
;; This means that the input is:
;; a BCond
;; (or CCond CCond)
;; (and CCond CCond)
;; (not CCond)
;; as defined. Returns false otherwise.
(define (isccond expr)
  (cond
    [ (isbcond expr) true ]
    [ (equal? (car expr) 'or)  (and (equal? (length expr) 3) (isccond (cadr expr)) (isccond (cadr (cdr expr)))) ]
    [ (equal? (car expr) 'and) (and (equal? (length expr) 3) (isccond (cadr expr)) (isccond (cadr (cdr expr)))) ]
    [ (equal? (car expr) 'not) (and (equal? (length expr) 2) (isccond (cadr expr))) ]
    [ else false ]
))

;; Returns true if the given expression is a valid BCond
;; by the rules of the grammar
;; This means that the input is:
;; (gt Expr Expr)
;; (lt Expr Expr)
;; (eq Expr Expr)
;; as defined. Returns false otherwise.
(define (isbcond expr)
  (cond
    [ (not (list? expr)) false ]
    [ (equal? (car expr) 'gt) (and (equal? (length expr) 3) (isexpr (cadr expr)) (isexpr (cadr (cdr expr)))) ]
    [ (equal? (car expr) 'lt) (and (equal? (length expr) 3) (isexpr (cadr expr)) (isexpr (cadr (cdr expr)))) ]
    [ (equal? (car expr) 'eq) (and (equal? (length expr) 3) (isexpr (cadr expr)) (isexpr (cadr (cdr expr)))) ]
    [ else false ]
    )
 )

;; Returns true if the given expression is a valid VarExpr
;; by the rules of the grammar
;; This means that the input is:
;; (var VarAssign Expr)
;; as defined. Returns false otherwise.
(define (isvarexpr expr)
  (cond
    [ (equal? (car expr) 'var) (and (equal? (length expr) 3) (isvarassign (cadr expr)) (isexpr (cadr (cdr expr)))) ]
    )
)

;; Returns true if the given expression is a valid VarAssign
;; by the rules of the grammar
;; This means that the input is:
;; (VarAssignSeq)
;; as defined. Returns false otherwise.
(define (isvarassign expr)
  (cond
    [ (isvarassignseq expr) true ]
    [ else false ]
  )
)

;; Returns true if the given expression is a valid VarAssignSeq
;; by the rules of the grammar
;; This means that the input is:
;; (Variable Expr)
;; (Variable Expr) VarAssignSeq
;; as defined. Returns false otherwise.
(define (isvarassignseq expr)
  (cond
    [ (not (pair? expr)) false ]
    [ (and (symbol? (car expr)) (isexpr (cadr expr))) true]
    [ (and (symbol? (car expr)) (isexpr (cadr expr)) (isvarassignseq (cadr (cdr expr)))) ]
    )
  )

;; Returns true if input is a valid arithmetic operator
;; (+,-,*,/)
(define (isarith x)
  (or (equal? x '+) (equal? x '-) (equal? x '*) (equal? x '/)))

;; Returns true if the first of a given pair is a conditional
;; (gt, lt, eq, or, and, not)
(define (iscond condition)
  (or (equal? (car condition) 'gt)
      (equal? (car condition) 'lt)
      (equal? (car condition) 'eq)
      (equal? (car condition) 'or)
      (equal? (car condition) 'and)
      (equal? (car condition) 'not)))

;; Returns true if the input equals 'var
;; Purely for style
(define (isvar x)
  (equal? x 'var))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;END GRAMMAR CHECK DEFINITONS ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;BEGIN EVALUATION DEFINITONS ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Attempt to evalate the expression.
;; Returns the output of the program if the input is valid,
;; 'Cannot Evalutate otherwise.
(define (eval expr env )
  (if (synchk expr env)
     (car (myeval expr env ))
     '(Cannot Evaluate))
  )

;; Evaluate an expression recursively depending on how it
;; is structured / parsed.
(define (myeval expr env)
  (cond
    [ (number? expr)  (list expr) ]
    [ (symbol? expr)  (list (findvalue expr env) ) ] 
    [ (isarith (car expr)) (list (evalarith expr env ) ) ]
    [ (isvarexpr expr) (list (evalvar expr env ) ) ]
    [ (isbcond expr) (list (evalbcond expr env) ) ]
    [ (isccond expr)  (if  (evalccond expr env )  
                               (list (myeval (cadr expr) env ) )
                               (list (myeval (cadr (cdr expr)) env )) ) ]
    [ else '(Cannot Evaluate) ]
    ))

;; Search for the value of a variable in the given environment
;; Input a symbol and environment
;; Output the value of the variable in the context of the environment
(define (findvalue var env)
  (if (equal? var (caar env)) 
      (cadr (car env))
      (findvalue var (cdr env))))

;; Evaluate an arithmetic expression
;; input an arithmetic expression and environment
;; output the value of the expression
(define (evalarith expr env)
  (cond
    ;;semantics of +
    [ (equal? (car expr) '+)
      (+ (car (myeval (cadr expr) env ))
         (car (myeval (cadr (cdr expr)) env))
      )
    ]
    ;;semantics of -
    [ (equal? (car expr) '-)
      (- (car (myeval (cadr expr) env ))
         (car (myeval (cadr (cdr expr)) env))
      )
    ]
    ;;semantics of *
    [ (equal? (car expr) '*)
      (* (car (myeval (cadr expr) env ))
         (car (myeval (cadr (cdr expr)) env))
      )
    ]
    ;;semantics of /
    [ (equal? (car expr) '/)
      (/ (car (myeval (cadr expr) env ))
         (car (myeval (cadr (cdr expr)) env))
      )
    ]
    ))

;; Evaluate a BCond expresison
;; input a BCond expression and variable
;; output a value 
(define (evalbcond expr env)
  (cond
    ;;semantics of gt
    [ (equal? (car expr) 'gt)
       (> (car (myeval (cadr expr) env ))
          (car (myeval (cadr (cdr expr)) env)))
       ]
     ;;semantics of lt
    [ (equal? (car expr) 'lt)
       (< (car (myeval (cadr expr) env ))
          (car (myeval (cadr (cdr expr)) env)))
       ]
     ;;semantics of eq
    [ (equal? (car expr) 'eq)
      (equal? (myeval (cadr expr) env) (myeval (cadr (cdr expr)) env))
      ]
  )
  )

;; Evaluate a ccond expression
(define (evalccond condition env )
  (cond
    ;;semantics of or
    [ (equal? (car condition) 'or)
       (or (car (myeval (cadr condition) env ))
          (car (myeval (cadr (cdr condition)) env)))
       ]
    ;;semantics of and
    [ (equal? (car condition) 'and)
       (and (car (myeval (cadr condition) env ))
          (car (myeval (cadr (cdr condition)) env)))
       ]
     ;;semantics of not    
    [ (equal? (car condition) 'not)
      (not (evalccond (cadr condition) env)) ]
    ))

;; Meant to take the variable expression, evaluate it,
;; then add it to the env, but it doesn't work.
(define (evalvar expr env )
  (myeval (caadr expr)
          (addvars (cadr expr) env))
  )

;; take in ((x 2) (y 3)) and return those prepended to the env
(define (addvars expr env)
  (if (equal? (length expr) 0) env
  (list*
   (list (caar expr) ;;add first var name
         (car (myeval (car (cdar expr)) env)))
   (addvars (cdr expr) env)
   )
  )
  )
