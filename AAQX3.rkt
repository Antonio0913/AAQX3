#lang typed/racket
(require typed/rackunit)

;;defining the AAQX3C language
(define-type AAQX3C (U numC binopC squareC funDefC idC appC ifleq0?))

(struct numC[(n : Real)] #:transparent)
(struct binopC[(op : Symbol) (l : AAQX3C) (r : AAQX3C)] #:transparent)
(struct squareC[(n : AAQX3C)] #:transparent)
(struct funDefC [(name : idC) (args : (Listof idC)) (body : AAQX3C)] #:transparent)
(struct idC [(name : Symbol)] #:transparent)
(struct appC [(name : idC) (args : (Listof AAQX3C))] #:transparent)
(struct ifleq0? [(test : AAQX3C) (then : AAQX3C) (else : AAQX3C)] #:transparent)

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


;;takes in _, _ and _ and 
(define (subst [subs : (Listof (Listof AAQX3C))] [in : AAQX3C] [fds : (Listof funDefC)]) : AAQX3C
  (match in
  [(numC n) in]
  [(idC s) (subst-id in subs fds)] ;;if it finds an idC it calls subst-id then just stops?
  ;;[(appC n a) (numC (interp (appC n (change-args subs a fds)) fds))]
  [(binopC op l r) (binopC op (subst subs l fds)
                      (subst subs r fds))]))
  

#;(define (change-args [subs : (Listof (Listof AAQX3C))] [args : (Listof AAQX3C)] [fds : (Listof funDefC)]) : (Listof AAQX3C)
  (match args 
    ['() '()]
    [(cons (? idC? id) r) (cons (subst-id id subs fds) (change-args subs r fds))]
    [(cons (? appC? app) r) (cons (subst subs app fds) (change-args subs r fds))]
    [(cons other r) (cons other (change-args subs r fds))]))




(define (zip [l1 : (Listof AAQX3C)] [l2 : (Listof AAQX3C)]) : (Listof (Listof AAQX3C))
  (cond
    [(not (equal? (length l1) (length l2))) (error 'zip "Number of variables and arguments do not match AAQX3")]
    [(empty? l1) '()]
    [else (cons (list (first l1) (first l2)) (zip (rest l1) (rest l2)))]))

#;(define (AAQX3C=? [arg1 : AAQX3C] [arg2 : AAQX3C]) : Boolean
  (equal? arg1 arg2))
 
(define (subst-id [s : idC] [subs : (Listof (Listof AAQX3C))] [fds : (Listof funDefC)]) : AAQX3C
  (match subs
    ['() (error 'subst-id (format "AAQX3 found an unbound variable: ~a" s))]
    [(cons (list what (? idC? for)) rest)
     (if (equal? s for)
         (numC (interp what fds))
         (subst-id s rest fds))]
    [(cons _ rest) (subst-id s rest fds)]))



;;function
(define (get-fundef [n : idC] [fds : (Listof funDefC)]) : funDefC
  (match fds
    ['() (error 'get-fundef "reference to undefined function AAQZ: ~a" n)]
    [(cons first-fd rest-fds) (cond
                   [(equal? n (funDefC-name first-fd)) first-fd]
                   [else (get-fundef n rest-fds)])]))

 
;;interpret functions
(define (interp-fns [funs : (Listof funDefC)]) : Real
  (printf "Function definitions (funs): ~a\n" funs)
  (interp (funDefC-body (find-main funs)) funs))

(define (find-main [funs : (Listof funDefC)]) : funDefC
  (match funs
    ['() (error 'find-main "AAQZ3C cant find main :(")]
    [(cons fun rest)
     (if (equal? (funDefC-name fun) (idC 'main)) fun (find-main rest))]))

 

(define (top-interp [funcs : Sexp])
  (interp-fns (parse-prog funcs)))

(define (interp [a : AAQX3C] [fds : (Listof funDefC)]) : Real
  (match a
    [(numC n) n]
    [(binopC op l r) ((hash-ref op-table op) (interp l fds) (interp r fds))]
    [(squareC n) (expt (interp n fds) 2)]
    [(ifleq0? test then else)
     (if (<= (interp test fds) 0)
         (interp then fds)
         (interp else fds))]
    [(appC f a) (local ([define fd (get-fundef f fds)])
              (interp (subst (zip a (funDefC-args fd))
                             (funDefC-body fd) fds) fds))] ;;passes in the whole function body?
    [(idC _) (error 'interp "AAQX3 shouldn't get here")]))



;;interp test cases
(check-equal? (interp n1 '()) 1)
(check-equal? (interp p1 '()) 3)
(check-equal? (interp m2 '()) 12)
(check-equal? (interp s1 '()) 4)

(check-equal? (interp (ifleq0? (numC 2) (numC 1) (numC 3)) '()) 3)
(check-equal? (interp (ifleq0? (numC -2) (numC 1) (numC 3)) '()) 1)

(check-equal? (interp (ifleq0? (binopC '- (numC 2) (numC 4)) (numC 1) (numC 3)) '()) 1)
(check-equal? (interp (ifleq0? (binopC '- (numC 2) (numC 4)) (binopC '* (numC 2) (numC 4)) (numC 3)) '()) 8)
 
;;parser


(define (parse-prog [progs : Sexp]) : (Listof funDefC)
  (match progs
    ['() '()]
    [(cons prog rest)
     (check-duplicate (parse-fundef prog) (parse-prog rest))]))
     #;(match prog
       [(list 'def (? symbol? name) '() '=> body) (cons (funDefC (idC name) '() (parse body)) (parse-prog rest))]
       [(list 'def (? symbol? name) (list args ...) '=> body)
        (if (andmap symbol? args) (cons (funDefC (idC name) (map idC args) (parse body)) (parse-prog rest))
             (error 'parse "AAQX3 Expected a list of symbols for arguments"))]
       [_ (error "AAQX3 Expected 'def' with correct syntax but found something else")])  
    ;;[_ (error "AAQX3 Invalid program format")]

;;takes in a function and a list of function and check if its a repeated name

(define (check-duplicate [new : funDefC] [existing : (Listof funDefC)]) : (Listof funDefC)
  (printf "Function definitions (funs): ~a\n" existing)
  (match existing
    ['() (cons new existing)]
    [(cons func rest)
     (if (equal? (funDefC-name new) (funDefC-name func))
                          (error "AAQZ3 found a syntax error repeated function name\n") (cons func (check-duplicate new rest)))]))


 

(define (parse-fundef [prog : Sexp]) : funDefC
  (match prog
    [(list 'def (? symbol? name) (list '() '=> body)) (funDefC (idC name) '() (parse body))]
    [(list 'def (? symbol? name) (list (list args ...) '=> body)) 
        (if (andmap symbol? args)  (funDefC (idC name) (map idC args) (parse body))
             (error 'parse "AAQX3 Expected a list of symbols for arguments"))])
  )

;;
(define (op-in-table? [op : Symbol]) : Boolean
  (hash-has-key? op-table op))

(define (parse [prog : Sexp]) : AAQX3C
  (match prog
    [(? real? n) (numC n)]
    [(list '^2 n) (squareC (parse n))]
    [(? symbol? s) (idC s)]
    [(list 'ifleq0? test then else) (ifleq0? (parse test) (parse then) (parse else))]
    [(list (? symbol? op) l r)
     (if (hash-has-key? op-table op)
         (binopC op (parse l) (parse r))
         ;(error 'parse "Unsupported operator in AAQX3: ~a" op)
         (appC (idC op) (list (parse l) (parse r))))]
    [(list (? symbol? s) args ...)
     (if (hash-has-key? op-table s)
         (error 'parse "Incorrect binop syntax: ~a in AAQX3" prog)
         (appC (idC s) (map parse args)))] ;;needs to be able to parse function calls too here right?
    [other (error 'parse "syntax error in AAQX3, got ~e" other)])) ;; when do we reach here now?


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
;(check-exn #rx"Unsupported operator" (lambda () (parse '{> 3 4})))

(check-exn #rx"Incorrect binop syntax:" (lambda () (parse '{+ 3})))

(check-equal? (parse '{ifleq0? 1 2 3})
              (ifleq0? (numC 1) (numC 2) (numC 3)))

(check-equal? (interp-fns
       (parse-prog '{{def f1 {(x y) => {+ x y}}}
                     {def main {() => {f1 1 2}}}}))
      3)

(check-equal? (interp-fns
        (parse-prog '{{def f {() => 5}}
                      {def main {() => {+ {f} {f}}}}}))
       10)


(check-exn #rx"AAQZ3 found a syntax error" (lambda () (interp-fns
        (parse-prog '{{def f {() => 5}}
                      {def f {() => 5}}
                      {def main {() => {+ {f} {f}}}}}))))

