;; 

#lang racket
(require racket/trace)
(provide (all-defined-out))
(require "hw5_t.rkt")
;;(require "program.rkt")


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
   

;; is op arithmatic operation
(define (arithop op)
  (or (equal? op '+)
      (equal? op '-)
      (equal? op '*)
      (equal? op '\))))

;;;;;;;;;;;;;;;;;;;;

(define (eval expr env heap)
  (if (null? (finderror env))
     (myeval expr env heap)
     (list (finderror env) heap)
     ))

(define (finderror env)
  (if (null? env)
      env ;;no error found
      (if (pair? env)
          (if (equal? (car env) 'exception)
              env
              (if (null? (finderror (car env)))
                  (finderror (cdr env))
                  (finderror (car env))
                  )
              )
              '()
          )
      )
  )

(define (myeval expr env heap)
  (cond
    
    [ (number? expr)  (list expr heap) ]
    [ (symbol? expr)  (list (findvalue expr env) heap) ] ;; new rule
    [ (equal? (car expr) 'exception) (list (expr env heap) ) ]
    [ (equal? (car expr) 'var) (evalvarassign (cadr expr) (cadr (cdr expr)) env heap) ]

       [(equal? (car expr) 'deref)
                           (if (number? (car (myeval (cadr expr) env heap)))
                           ;;get the value
                           (list (readfromheap
                           ;;the result of the eval
                           (car (myeval (cadr expr) env heap))
                           ;;heap
                           heap) heap)
                           (myeval (cadr (cdr expr)) env heap))
     ]
    [(equal? (car expr) 'ref)
                          ;;check for exceptions
                          (if (equal? (car (myeval (cadr expr) env heap)) 'exception)
                              (myeval (cadr (cdr expr)) env heap)
                          ;;if a free location exists then write to heap, else exception
                          (if (number? (findfree (cadr (myeval (cadr expr) env heap))))
                                       (list (findfree (cadr (myeval (cadr expr) env heap)))
                                             ;;return and write to the location
                                             (writetoheap
                                              (findfree (cadr (myeval (cadr expr) env heap)))
                                              (car (myeval (cadr expr) env heap))
                                              (cadr (myeval (cadr expr) env heap))
                                              false))
                              (list '(exception oom) heap)))
     ]
    
    [(equal? (car expr) 'free)
                          (if (null? heap) ;; can't free a null heap
                              (list '(exception ooma) heap)
                          ;;check for exceptions
                          (if (equal? (car (myeval (cadr expr) env heap)) 'exception)
                              (myeval (cadr (cdr expr)) env heap)
                          ;;if heap location is already free, fma exception
                          (if (number? (findfree (cadr (myeval (cadr expr) env heap))))
                              (list '(exception fma) heap)
                               (list (car (myeval (cadr expr) env heap))
                                ;;return free the location
                                     (writetoheap
                                         (cadr expr)
                                         'free
                                         (cadr (myeval (cadr expr) env heap))
                                         false)
                                     )
                          )))
     ]

    [(equal? (car expr) 'wref)
               ;;broken exception handling
               (if (equal? (car (myeval (cadr (cdr expr)) env heap)) 'exception)
                   (myeval (cadr (cdr expr)) env heap)
                   (if (equal? (car (writetoheap (car (myeval (cadr expr) env heap))
                                           (car (myeval (cadr (cdr expr)) env heap))
                                           (cadr (myeval (cadr (cdr expr)) env heap))
                                           true)) 'exception)
                       (list (writetoheap (car (myeval (cadr expr) env heap))
                                           (car (myeval (cadr (cdr expr)) env heap))
                                           (cadr (myeval (cadr (cdr expr)) env heap))
                                           true) heap)
                              ;;list the value and the heap together 
                              (list (car (myeval (cadr (cdr expr)) env heap))
                              (writetoheap (car (myeval (cadr expr) env heap))
                                           (car (myeval (cadr (cdr expr)) env heap))
                                           (cadr (myeval (cadr (cdr expr)) env heap))
                                           true))))
     ]
  
    [ (arithop (car expr)) (evalarith (car expr) (cadr expr) (cadr (cdr expr)) env heap) ]
        [ else  (ifthenelse (evalcond (car expr) env heap) 
                            (eval (cadr expr) env heap)
                            (eval (cadr (cdr expr)) env heap)) ]
   
    ))

(define (isref expr)
  (equal? (car expr) 'ref)
  (equal? (car expr) 'wref)
  (equal? (car expr) 'deref))

(define (findvalue var env)
  (if (equal? var (car (car env))) ;; We already know expression does not contain free variables
      (cadr (car env))
      (findvalue var (cdr env))))


(define (isarith x)
  (or (equal? x '+) (equal? x '-) (equal? x '*) (equal? x '/)))

;; input: arithoperator, expr-operand1, expr-operand2, env
;; output: '(Cannot Evaluate) or some number
;; used: myapply 
(define (evalarith op expr1 expr2 env heap)
 (myapply op (car (eval expr1 env heap)) (car (eval  expr2 env heap)) heap))

;; input: some operator, arithmatic or conditional
;;        operand-values for the operator
;; output: '(Cannot Evaluate) or number or boolean 
(define (myapply op val1 val2 heap)
  (if (or (equal? val1 '(Cannot Evaluate))
          (equal? val2 '(Cannot Evaluate)))
      '(Cannot Evaluate)
      (if (null? (finderror (list val1 val2)))
          (cond
            [ (equal? op '+) (+ val1 val2) ]
            [ (equal? op '-) (- val1 val2) ]
            [ (equal? op '*) (* val1 val2) ]
            [ (equal? op 'gt) (> val1 val2) ]
            [ (equal? op 'lt) (< val1 val2) ]
            [ (equal? op 'eq) (equal? val1 val2) ]
            [ (equal? op 'and) (and val1 val2) ]
            [ (equal? op 'or) (or val1 val2) ]
            [ (equal? op 'not) (not val1) ])
          (finderror (list val1 val2))
      )))

(define (iscond condition)
  (and (pair? condition)
       (or (equal? (car condition) 'gt)
           (equal? (car condition) 'lt)
           (equal? (car condition) 'eq)
           (equal? (car condition) 'or)
           (equal? (car condition) 'and)
           (equal? (car condition) 'not))))


;; input: conditions of the form (gt/lt/eq expr1 expr2), (or/and cond1 cond2), (not cond)
;; output: true/false, '(Cannot Evaluate)
;; used: myapply
(define (evalcond condexpr env heap)
  (cond
    [ (equal? (car condexpr) 'gt)
      (myapply 'gt (eval (cadr condexpr) env heap) (eval (cadr (cdr condexpr)) env heap) heap) ]
    
    [ (equal? (car condexpr) 'lt)
      (myapply 'lt (eval (cadr condexpr) env heap) (eval (cadr (cdr condexpr)) env heap) heap) ]
    
    [ (equal? (car condexpr) 'and)
      (myapply 'and (evalcond (cadr condexpr) env heap)
               (evalcond (cadr (cdr condexpr)) env heap) heap) ]

    [ (equal? (car condexpr) 'or)
      (myapply 'or (evalcond (cadr condexpr) env heap)
               (evalcond (cadr (cdr condexpr)) env heap) heap) ]

    [ (equal? (car condexpr) 'not)
      (myapply 'not (evalcond (cadr condexpr) env heap)
               false) ] ;; dummy
    )
  )

;; input: true/false, '(Cannot Evaluate) expression values
;; output: '(Cannot Evaluate) or expression values
;;         expression values can be '(Cannot Evaluate)
(define (ifthenelse condition expr1 expr2)
  (if (equal? condition '(Cannot Evaluate))
      '(Cannot Evaluate)
      (if condition
          expr1
          expr2)))

;; input: list of (variable expression), expr to evaluate, environment
;; output: evaluate expr in some environment

(define (evalvarassign varassigns expr env heap)
  (if (null? varassigns)  ;; no variable expression pair, 
      (eval expr env heap)     ;; then just evaluate the expression in the current environment
      ;; else
      ;; recursive call with the suffix of varassigns, with the same expr
      ;; in the environment constructed by cons-ing (variable evaluation of expression)
      ;; to the given environment env.
      (evalvarassign (cdr varassigns)
                     expr
                     (cons (list (car (car varassigns))
                                 (eval (cadr (car varassigns)) env heap))
                           env)
                     heap)))


;; writes at the location loc the integer value val
;; Precondition: loc is integer
;;               val is integer
;;               heap is a syntactically correct representation of heap as a
;;                  list of pairs, where each element in the pair is an integer
;;                  and no two pair has the same integer as the first element
;;               flag is boolean: true to disallow writing to free location
;;                                false to allow writing to free location
(define (writetoheap loc val heap flag)
  (if (null? heap)
      '(exception ooma) ;; Out of memory access exception
      (if (equal? (car (car heap)) loc)                                   ;; if location found
          (if (and flag (equal? (cadr (car heap)) 'free))                 ;;   if location is free
              '(exception fma)                                            ;;     free memory access exception
              (cons (list (car (car heap)) val) (cdr heap)))              ;;    else update that value stored at that location
          (if (equal? (writetoheap loc val (cdr heap) flag) '(exception ooma)) ;; else search for location in the rest
              '(exception ooma)                                           ;;    exception pushback
              (if (equal? (writetoheap loc val (cdr heap) flag) '(exception fma))
                  '(exception fma)
                  (cons (car heap) (writetoheap loc val (cdr heap) flag))))))) ;;    construct the new heap and return


;; read the value stored at the location loc
;; Precondition: loc is integer
;;               heap is a syntactically correct representation of heap as a
;;                  list if pairs, where each element in the pair is an integer
;;                  and no two pair has the same integer as the first element
(define (readfromheap loc heap)
  (if (null? heap)
      '(exception ooma) ;; Out of memory access exception
      (if (equal? (car (car heap)) loc)          ;; if location found
          (if (equal? (cadr (car heap)) 'free)   ;;   if location is free location
              '(exception fma)                   ;;      free memory access exception: nullpointer
              (cadr (car heap)))                 ;;    otherwise return the value
          (readfromheap loc (cdr heap)))))       ;; otherwise continue the search

;; helper functions for references
;; Precondition: 
;;               heap is a syntactically correct representation of heap as a
;;                  list if pairs, where each element in the pair is an integer
;;                  and no two pair has the same integer as the first element
(define (findfree heap)
  (if (null? heap)
      '(exception oom) ;; Out of memory exception
      (if (equal? (cadr (car heap)) 'free)  ;; if the location is free
          (car (car heap))                  ;;    return the location 
          (findfree (cdr heap)))))          ;; else continue looking for free


(define prog1 '(var ((x (deref 1))) (+ x 1)))
(define prog2 '(var ((x (ref 32)))(var ((y (+ x 1)))(deref y))))


(trace myeval)
(trace findfree)
(trace readfromheap)
(trace writetoheap)
(trace evalarith)
(trace eval) 
(trace evalcond)
(trace finderror)

