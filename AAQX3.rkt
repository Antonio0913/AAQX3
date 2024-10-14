#lang typed/racket
(require typed/rackunit)

;;defining the Arith language
(define-type AAQX3C (U numC binopC squareC funDefC idC))

(struct numC[(n : Real)] #:transparent)
(struct binopC[(op : Symbol) (l : AAQX3C) (r : AAQX3C)] #:transparent)
(struct squareC[(n : AAQX3C)] #:transparent)
(struct funDefC [(name : Symbol) (args : (Listof Symbol)) (body : AAQX3C)] #:transparent)
(struct idC [(name : Symbol)] #:transparent)
(struct appC [(name : Symbol) (args : AAQX3C)] #:transparent)

(define n1 (numC 1))
(define n2 (numC 2))
(define n3 (numC 3))
(define n4 (numC 4))

(define p1 (binopC '+ n1 n2))
(define p2 (binopC '+ n3 n4))

(define m1 (binopC '* n1 n2))
(define m2 (binopC '* n3 n4))

(define s1 (squareC n2))
(define s2 (squareC n3))




;;defining hash table for operations
(define op-table
  (hash
    '+ +
    '* *
    '/ /
    '- -))

;;substs
(define (subst [what : AAQX3C] [for : Symbol] [in : AAQX3C]) : AAQX3C
  (match in
  [(numC n) in]
  [(idC s) (cond
             [(symbol=? s for) what]
             [else in])]
  [(appC n a) (appC n (subst what for a))]
  [(binopC op l r) (binopC op (subst what for l)
                      (subst what for r))]))

;;function
(define (get-fundef [n : Symbol] [fds : (Listof funDefC)]) : funDefC
  (cond
    [(empty? fds) (error 'get-fundef "reference to undefined function AAQZ")]
    [(cons? fds) (cond
                   [(equal? n (funDefC-name (first fds))) (first fds)]
                   [else (get-fundef n (rest fds))])]))

;;interpret function
(define (interp [a : AAQX3C] [fds : (Listof funDefC)]) : Real
  (match a
    [(numC n) n]
    [(binopC op l r) ((hash-ref op-table op) (interp l fds) (interp r fds))]
    [(squareC n) (expt (interp n fds) 2)]
    [(appC f a) (local ([define fd (get-fundef f fds)])
              (interp (subst a
                             (funDefC-args fd)
                             (funDefC-body fd))
                      fds))]
    [(idC _) (error 'interp "AAQZ shouldn't get here")]))
 
;;interp test cases
(check-equal? (interp n1 '()) 1)
(check-equal? (interp p1 '()) 3)
(check-equal? (interp m2 '()) 12)
(check-equal? (interp s1 '()) 4)
  
;;parser
(define (parse [prog : Sexp]) : AAQX3C
  (match prog
    [(? real? n) (numC n)]
    [(? symbol? s) (idC s)]
    [(list 'def (? symbol? name) '=> body) (funDefC name '() (parse body))]
    [(list 'def (? symbol? name) (list (? symbol? args) ...) '=> body) (funDefC name (cast args (Listof Symbol)) (parse body))]
    [(list (? symbol? op) l r)
     (if (hash-has-key? op-table op)
         (binopC op (parse l) (parse r))
         (error 'parse "Unsupported operator in AAQZ: ~a" op))]
    [(list '^2 n) (squareC (parse n))]
    [other (error 'parse "syntax error in AAQZ, got ~e" other)]))

;;parse test cases

(check-equal? (parse '5)
             (numC 5))

(check-equal? (parse '{^2 {+ 4 5}})
              (squareC
               (binopC '+ (numC 4) (numC 5))))
              

(check-equal? (parse '{* {+ 2 3} 5})
              (binopC '*
                     (binopC '+ (numC 2) (numC 3))
                     (numC 5)))
(check-exn #rx"Unsupported operator" (lambda () (parse '{> 3 4})))
(check-exn #rx"syntax error" (lambda () (parse '{+ 3})))

(check-equal? (parse '{def addOne (x) =>
                                     (+ x 1)})
              (funDefC 'addOne '(x) (binopC '+ (idC 'x) (numC 1))))

(check-equal? (parse '{def oneAddOne =>
                                     (+ 1 1)})
              (funDefC 'oneAddOne '() (binopC '+ (numC '1) (numC 1))))

(check-equal? (parse '{def area (w h) =>
                                       (* w h)})
                     (funDefC 'area '(w h) (binopC '* (idC 'w) (idC 'h))))









