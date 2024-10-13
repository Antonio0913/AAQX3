#lang typed/racket


;;define type
(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)]
  [squareC (n : number)])

(define (interp [a : ArithC]) : number
  (type-case ArithC a
    [numC (n) n]
    [plusC (l r) (+ (interp l) (interp r))]
    [multC (l r) (* ( interp l) (interp r))]
    [squareC (n) (* n n)]))

(define (parser [args : Sexp])
  (match args
    [(list (? number? n)) (numC n)]
    [(list '^2 (? number? n)) (squareC n)]
    [(list (? number? l) '+ (? number? r)) (plusC l r)]
    [(list (? number? l) '* (? number? r)) (multC l r)]
    [other (error 'invalid-syntax "Invalid syntax provided: ~a" x)]))