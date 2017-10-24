#lang racket
(require racket/trace)
(require "program.rkt")
;(require "helperheap.rkt")
(provide (all-defined-out))



;; to make sure I remember what I am using
(define (value result) (car result))
(define (heap result)  (cadr result))

;; returns '(value/exception heap)
(define (eval expr env heap)
  (cond
    ;; number
    [ (number? expr)  (list expr heap) ] 

    ;; variable
    [ (symbol? expr)  (list (findvalue expr env) heap) ]

    ;; arithexpression: use evalarith
    [ (arithop (car expr))
      ;; then
      (evalarith (car expr) (cadr expr) (cadr (cdr expr)) env heap) ]

    ;; varexpression
    [ (equal? (car expr) 'var)
      ;; then
      (evalvarassign (cadr expr) (cadr (cdr expr)) env heap) ]

    ;; fun expression
    [ (equal? (car expr) 'fun)
      ;; then just add the function definition to the environment
      (eval (cadr (cdr expr)) (cons (cadr expr) env) heap) ]

    ;; application expression: use translate
    [ (equal? (car expr) 'apply)
      ;; then translate to varexpressions
      (eval (translate (cadr expr) env) env heap) ]

    ;; deref: use derefhandler - we will just pass the values there as
    ;; expressions inside deref cannot produce any exceptions 
    [ (equal? (car expr) 'deref)
      ;; then read from the heap
      (derefhandler (value (eval (cadr expr) env heap)) heap) ]

    ;; wref: use wrefhandler
    [ (equal? (car expr) 'wref)
      ;; then
      (wrefhandler (value (eval (cadr expr) env heap)) (value (eval (cadr (cdr expr)) env heap)) heap) ]

    ;; free: use freehandler
    [ (equal? (car expr) 'free)
      ;; then
      (freehandler (value (eval (cadr expr) env heap)) heap) ]

    ;; ref: use refhandler
    [ (equal? (car expr) 'ref)
      ;; then
      (refhandler (value (eval (cadr expr) env heap)) heap) ]

    ;; nothing else left: must be a cond expression
    [ else  (ifthenelse (car expr) (cadr expr) (cadr (cdr expr)) env heap) ]
    ))

;; helpers
;; input: variable, environment
;; output: value to which the variable is mapped to in the environment
;;         It can be '(Cannot Evaluate) 
(define (findvalue x env)
  (if (null? env)  
      '(Cannot Evaluate)
      (if (equal? (car (car env)) x)
          (cadr (car env))
          (findvalue x (cdr env)))))

;; is op arithmatic operation
(define (arithop op)
  (or (equal? op '+)
      (equal? op '-)
      (equal? op '*)
      (equal? op '\))))

;; evalarith
;; input: op = +, -, ...; others self explanatory
;; output: (value/exception heap)
(define (evalarith op expr1 expr2 env heap)
  (evalarith1 op (eval expr1 env heap) expr2 env))  ;; evaluate in one function and use in another
                                              ;; to avoid unnecessary recomputation

(define (evalarith1 op semexpr1 expr2 env)
  (if (list? (value semexpr1)) ;; expr1 evaluation produced an exception
      semexpr1
      (evalarith2 op (value semexpr1) (eval expr2 env (heap semexpr1)))))

(define (evalarith2 op valexpr1 semexpr2)
  (if (list? (value semexpr2)) ;; expr2 evaluation produced an exception
      semexpr2
      ;; else use myapply as before and pair it with the heap
      (list (myapply op valexpr1 (value semexpr2)) (heap semexpr2))))

;; evalvarassign
;; input: list of (variable expression), expr to evaluate, environment, heap
;; output: evaluate expr in new environment and heap or exception in between
(define (evalvarassign varassigns expr env heap)
  (if (null? varassigns)
      (eval expr env heap)
      ;; else: use the same method as above to avoid recomputations
      (evalvarassign1 (car (car varassigns))
                      (eval (cadr (car varassigns)) env heap)
                      (cdr varassigns)
                      expr
                      env)))
;; input: variable (value/exception heap) varassigns expr env
;; output: evaluate expr in new env and heap (mutual recursion with evalvarassign)
(define (evalvarassign1 variable sem varassigns expr env)
  (if (list? (car sem)) ;; exception already!!
      sem
      ;; call evalvarassign with the rest of varassigns
      ;; create the correct environment and heap
      (evalvarassign varassigns expr (cons (list variable (value sem)) env) (heap sem))))


;; translate applications to var-expressions as discussed in class
;; input: expr = (f ArgList)
(define (translate expr env)
  (translate1 (findfun (car expr) (cadr expr) env)))

;; env ((f ParamList) fDef)
(define (findfun fname args env)
  (if (and (list? (car (car env))) ;; here is a function
           (equal? (car (car (car env))) fname) ;; name matches
           (equal? (length (cadr (car (car env)))) (length args))) ;; arg size matches
      (list (cadr (car (car env)))  ;; parameter list
            args                    ;; arg list
            (cadr (car env)))       ;; body of the function to evaluate
      ;; else
      (findfun fname args (cdr env))))


(define (translate1 triplet)
  (list 'var (translate2 (car triplet) (cadr triplet)) (cadr (cdr triplet))))

;; input: paramlist, arglist, expression to evaluate
(define (translate2 paramlist arglist)
  (if (null? paramlist)
      '()
      (cons (list (car paramlist) (car arglist))
            (translate2 (cdr paramlist) (cdr arglist)))))

;; Now the pointer operations
;; deref
;; input: location and heap
;; output: value at location/exception paired with heap 
(define (derefhandler location heap)
    (if (null? heap)
        (list '(exception ooma) heap)
        (if (equal? (car (car heap)) location)
            (if (equal? (cadr (car heap)) 'free)
                (list '(exception fma) heap)
                (list (cadr (car heap)) heap))
            (mycons (car heap) (derefhandler location (cdr heap)))))) ;;; mycons will add the element to the second element of pair returned.

;; wref
;; input: location, value, heap
;; output: value written at location/exception heap
(define (wrefhandler location value heap)
  (if (null? heap)
      (list '(exception ooma) heap)
      (if (equal? (car (car heap)) location)
          (if (equal? (cadr (car heap)) 'free)
              (list '(exception fma) heap)
              (list value (cons (list location value) (cdr heap))))
          (mycons (car heap) (wrefhandler location value (cdr heap)))))) 

(define (mycons el pair)
  (list (car pair) (cons el (cadr pair))))

;; input: location to free
;; output: location/exception heap
(define (freehandler location heap)
  (if (null? heap)
      (list '(exception ooma) heap)
      (if (equal? (car (car heap)) location)
          (if (equal? (cadr (car heap)) 'free)
              (list '(exception fma) heap)
              (list location (cons (list location 'free) (cdr heap))))
          (mycons (car heap) (freehandler location (cdr heap))))))

(define (refhandler value heap)
  (if (null? heap)
      (list '(exception oom) heap)
      (if (equal? (cadr (car heap)) 'free) ;; found free location
          (list (car (car heap)) ;; location
                (cons (list (car (car heap)) value) (cdr heap)))
          (mycons (car heap) (refhandler value (cdr heap))))))

;; input: location, value, heap
;; output: value written at location/exception heap

;; input: condition thenexpr elseexpr env heap
;; output: value/exception heap
;; same method as in evalarith
(define (ifthenelse condition thenexpr elseexpr env heap)
  (ifthenelse1 (evalcond condition env heap) thenexpr elseexpr env))

(define (ifthenelse1 semcond thenexpr elseexpr env)
  (if (list? (value semcond)) ;; that means exception
      semcond
      (if (value semcond) ;; condition is true
          (eval thenexpr env (heap semcond))
          (eval elseexpr env (heap semcond)))))

;; input: gt/lt/eq/and/or/not env heap
;; output: true/false/exception heap
;; same method as above: divide and conquer
(define (evalcond condition env heap)
  (if (equal? (car condition) 'not)
      (evalcond1 (car condition) (cadr condition) 'dummy env heap) ;; not just an one operand
      (evalcond1 (car condition) (cadr condition) (cadr (cdr condition)) env heap)))

(define (evalcond1 op operand1 operand2 env heap)
  (cond
   [ (or (equal? op 'gt)
         (equal? op 'lt)
         (equal? op 'eq))
     (evalcond2 op (eval operand1 env heap) operand2 env) ] ;; basic conditions: call evals
   [ else (evalcond2 op (evalcond operand1 env heap) operand2 env) ] ;; call evalconds
   ))

(define (evalcond2 op semoperand1 operand2 env)
  (if (list? (value semoperand1)) ;; exception
      semoperand1
      (if (equal? operand2 'dummy) ;; op is not
          (list (myapply op (value semoperand1) 'dummy) (heap semoperand1))
          (cond
            [ (or (equal? op 'gt)
                  (equal? op 'lt)
                  (equal? op 'eq))
              (evalcond3 op (value semoperand1) (eval operand2 env (heap semoperand1))) ] ;; basic conditions
            [ else
              (evalcond3 op (value semoperand1) (evalcond operand2 env (heap semoperand1))) ]
            ))))
                  

(define (evalcond3 op valoperand1 semoperand2)
  (if (list? (value semoperand2)) ;; exception
      semoperand2
      (list (myapply op valoperand1 (value semoperand2)) (heap semoperand2))))


(define (myapply op val1 val2)
  (cond
    [ (equal? op '+) (+ val1 val2) ]
    [ (equal? op '-) (- val1 val2) ]
    [ (equal? op '*) (* val1 val2) ]
    [ (equal? op 'gt) (> val1 val2) ]
    [ (equal? op 'lt) (< val1 val2) ]
    [ (equal? op 'eq) (equal? val1 val2) ]
    [ (equal? op 'and) (and val1 val2) ]
    [ (equal? op 'or) (or val1 val2) ]
    [ (equal? op 'not) (not val1) ]))  
                   
;(trace eval)


                  