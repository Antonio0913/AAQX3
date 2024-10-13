#lang typed/racket
(require typed/rackunit)

;;defining the Arith language
(define-type ArithC (U numC plusC multC squareC))

(struct numC[(n : Real)] #:transparent)
(struct plusC[(l : ArithC) (r : ArithC)] #:transparent)
(struct multC[(l : ArithC) (r : ArithC)] #:transparent)
(struct squareC[(n : ArithC)] #:transparent)

(define (interp [a : ArithC]) : Real
  (match a
    [(numC n) n]
    [(plusC l r) (+ (interp l) (interp r))]
    [(multC l r) (* (interp l) (interp r))]
    [(squareC n) (expt (interp n) 2)]))

(define (parse [prog : Sexp]) : ArithC
  (match prog
    [(? real? n) (numC n)]
    [(list '+ l r) (plusC (parse l) (parse r) )]
    [(list '* l r) (multC (parse l) (parse r) )]
    [(list '^2 n) (squareC (parse n))]
    [other (error 'parse "syntax error, got ~e" other)]))


;; defining numC, plusC, multC, and squareC vars for testing
(define n1 (numC 5))
(define n2 (numC 4))
(define n3 (numC 3))
(define n4 (numC 2))

(define p1 (plusC n1 n2))
(define p2 (plusC n3 n4))
(define p3 (plusC p1 p2))

(define m1 (multC n1 n2))
(define m2 (multC n3 n4))
(define m3 (multC m1 m2))

(define s1 (squareC n1))


(check-equal? (interp p1) 9)
(check-equal? (interp m1) 20)
(check-equal? (interp s1) 25)
(check-equal? (interp (squareC p1)) 81)
(check-equal? (interp (squareC m1)) 400)



(check-equal? (parse '{* {+ 2 3} 5})
              (multC (plusC (numC 2) (numC 3))
                     (numC 5)))

(check-equal? (parse '1) (numC 1))
(check-equal? (parse '{+ 1 2})
              (plusC (numC 1) (numC 2)))
(check-equal? (parse '{* 1 2})
              (multC (numC 1) (numC 2)))

(check-equal? (parse '{^2 4}) (squareC (numC 4)))

(check-exn #rx"syntax error" (lambda () (parse '{+ 1})))

