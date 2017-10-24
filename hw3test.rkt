;; For homework assignment 3: Spring'17

#lang racket
(provide (all-defined-out))

;; For syntax checking
;; 12 points syntax checking
;; 

;; arithmatic expression has two arguments
(define prog1
  '(+ x y a))

(define prog2
  '(+ (+ x y) (+ y z z)))

;; bcond has three arguments
(define prog3
  '((gt (+ x y) 2) 1 2 3))

;; ccond does not have correct arguments
(define prog4
  '((gt (+ x ((or (gt x y) ((lt y z) 1 2)) 1 2)) 2) 1 2))

;; varassignpair is a pair
(define prog5
  '(+ 1 (var ((x 0) (y 0 1)) 1)))

;; there is no pair 
(define prog6
  '(+ 1 (var () 1)))

(define prog7
  '(+ 1 (var (()) 1)))

;; all evals will also include synchk
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 20 2pt
(define prog8
  '(var ((x 10) (y x)) (+ x y)))

(define env8 '((x 20)))
(define env8a '())

;; -100  2pt
;; cannot evaluate 2pt
(define prog9
  '((gt (var ((x 10)) (+ x y)) 20) (- x y) (+ x y)))
(define env9 '((x 100) (y 200)))
(define env9a '((x 100)))  

;; conditional expression containing variable assignment
;; 31 2pt
;; cannot evaluate 2pt
(define prog10
  '((or (gt (var ((x 10)) (+ x y)) 20)
        (gt (var ((x 20)) (+ x z)) 20))
    (var ((y 10) (z 20)) (+ x (+ y z)))
    (+ x (+ y z))))
(define env10 '((x 1) (y 2) (z 3)))
(define env10a '())

;; variable is assigned to the result of conditional expression
;; 44 3pt
;; cannot evaluate 3pt
(define prog11
  '(var ((x ((or (gt (var ((x 10)) (+ x y)) 20)
                (gt (var ((x 20)) (+ x z)) 20))
            (var ((y 10) (z 20)) (+ x (+ y z)))
            (+ x (+ y z)))))
         (+ x (var ((y 10)) (+ z y)))))
(define env11 '((x 1) (y 2) (z 3)))
(define env11a '((x 1) (y 2)))

;; variable assignment using another variable assignment
;; -20   3pt
;; -20   2pt 
;; cannot evaluate 1pt
(define prog12
  '(var ((x (var ((x 10)) (+ x y)))
         (y (+ x 20)))
        ((gt x y) (+ x y) (- x y))))
(define env12 '((x 1) (y 2)))
(define env12a '((y 1000) (z 2)))
(define env12b '())