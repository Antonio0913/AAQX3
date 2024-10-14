#lang typed/racket
(require typed/rackunit)

;;defining the Arith language
(define-type AAQX3C (U numC binopC squareC funDefC))

(struct numC[(n : Real)] #:transparent)
(struct binopC[(op : Symbol) (l : AAQX3C) (r : AAQX3C)] #:transparent)
(struct squareC[(n : AAQX3C)] #:transparent)
(struct funDefC [(name : Symbol) (args : (Listof Symbol)) (body : AAQX3C)] #:transparent)

;;defining hash table for operations
(define op-table
  (hash
    '+ +
    '* *
    '/ /
    '- -))

;;interpret function
(define (interp [a : AAQX3C]) : Real
  (match a
    [(numC n) n]
    [(binopC op l r) ((hash-ref op-table op) (interp l) (interp r))]
    [(squareC n) (expt (interp n) 2)]))




;;parser
(define (parse [prog : Sexp]) : AAQX3C
  (match prog
    [(? real? n) (numC n)]
    [(list (? symbol? op) (? real? l) (? real? r))
     (if (hash-has-key? op-table op)
         (binopC op (parse l) (parse r))
         (error 'parse "Unsupported operator: ~a" op))]
    [(list '^2 (? real? n)) (squareC (parse n))]
    [(list 'def n (list args ...) '=> body) (funDefC n args (parse body))]
    [other (error 'parse "syntax error, got ~e" other)]))









