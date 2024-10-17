#lang typed/racket
(require typed/rackunit)

;;defining the AAQZ3 language
(define-type ExprC (U numC binopC FundefC idC appC ifleq0?))

(struct numC[(n : Real)] #:transparent)
(struct binopC[(op : Symbol) (l : ExprC) (r : ExprC)] #:transparent)
(struct FundefC [(name : idC) (args : (Listof idC)) (body : ExprC)] #:transparent)
(struct idC [(name : Symbol)] #:transparent)
(struct appC [(name : idC) (args : (Listof ExprC))] #:transparent)
(struct ifleq0? [(test : ExprC) (then : ExprC) (else : ExprC)] #:transparent)

(define n1 (numC 1))
(define n2 (numC 2))
(define n3 (numC 3))
(define n4 (numC 4))

(define p1 (binopC '+ n1 n2))
(define p2 (binopC '+ n3 n4))

(define m1 (binopC '* n1 n2))
(define m2 (binopC '* n3 n4))





;;defining hash table for operations
(define op-table
  (hash
    '+ +
    '* *
    '/ /
    '- -))

;;defining hash table for invalid identifier
(define invalid-table
  (hash
   '+ 0
   '* 0
   '/ 0
   '- 0
   'def 0 
   'ifleq0? 0
   '=> 0))


;;takes in _, _ and _ and 
(define (subst [subs : (Listof (Listof ExprC))] [in : ExprC] [fds : (Listof FundefC)]) : ExprC
  (printf "in ~a\n" in)
  (match in
  [(numC n) in]
  [(idC s) (subst-id in subs fds)] ;;if it finds an idC it calls subst-id then just stops?
  
  [(binopC op l r) (binopC op (subst subs l fds)
                      (subst subs r fds))]
  [(appC n a) (numC (interp (appC n (change-args subs a fds)) fds))]
  [(ifleq0? con f1 f2) (ifleq0? (subst subs con fds) (subst subs f1 fds) (subst subs f2 fds))]))
  

#;(define (change-args [subs : (Listof (Listof ExprC))] [args : (Listof ExprC)]
                       [fds : (Listof FundefC)]) : (Listof ExprC)
  (printf "change-args: ~a\n" subs)
(match args 
    ['() '()]
    [(cons (? idC? id) r) (cons (subst-id id subs fds) (change-args subs r fds))]
    [(cons (? appC? app) r) (cons (subst subs app fds) (change-args subs r fds))]
    [(cons other r) (cons other (change-args subs r fds))])) 

 (define (change-args [subs : (Listof (Listof ExprC))] [args : (Listof ExprC)]
                      [fds : (Listof FundefC)]) : (Listof ExprC)
   (match args 
    ['() '()]
    [(cons (? idC? id) r) 
     (cons (subst-id id subs fds) (change-args subs r fds))]
    [(cons other r) 
     (cons (subst subs other fds) (change-args subs r fds))]))


(define (zip [l1 : (Listof ExprC)] [l2 : (Listof ExprC)]) : (Listof (Listof ExprC))
  (match (list l1 l2)
    [(list '() '()) '()]
    [(list (cons f1 r1) (cons f2 r2)) (cons (list f1 f2) (zip r1 r2))]
    [other (error 'zip "Number of variables and arguments do not match AAQZ3")]))

#;(define (ExprC=? [arg1 : ExprC] [arg2 : ExprC]) : Boolean
  (equal? arg1 arg2))
 
(define (subst-id [s : idC] [subs : (Listof (Listof ExprC))] [fds : (Listof FundefC)]) : ExprC
  (match subs
    ['() (error 'subst-id (format "AAQZ3 found an unbound variable: ~a" s))]
    [(cons (list what (? idC? for)) rest)
     (if (equal? s for)
         (numC (interp what fds))
         (subst-id s rest fds))]))
    ;;[(cons _ rest) (subst-id s rest fds)]))

 

;;function
(define (get-fundef [n : idC] [fds : (Listof FundefC)]) : FundefC
  (match fds
    ['() (error 'get-fundef "reference to undefined function AAQZ: ~a" n)]
    [(cons first-fd rest-fds) (cond
                   [(equal? n (FundefC-name first-fd)) first-fd]
                   [else (get-fundef n rest-fds)])]))

 
;;interpret functions
(define (interp-fns [funs : (Listof FundefC)]) : Real
  (interp (FundefC-body (find-main funs)) funs))

(define (find-main [funs : (Listof FundefC)]) : FundefC
  (match funs
    ['() (error 'find-main "AAQZ3C cant find main :(")]
    [(cons fun rest)
     (if (equal? (FundefC-name fun) (idC 'main)) fun (find-main rest))]))

 

(define (top-interp [funcs : Sexp])
  (interp-fns (parse-prog funcs)))

(define (interp [a : ExprC] [fds : (Listof FundefC)]) : Real 
  (match a
    [(numC n) n]
    [(binopC op l r) ((hash-ref op-table op) (interp l fds) (interp r fds))]
    [(ifleq0? test then else)
     (if (<= (interp test fds) 0)
         (interp then fds)
         (interp else fds))]
    [(appC f a) (local ([define fd (get-fundef f fds)])
              (interp (subst (zip a (FundefC-args fd))
                             (FundefC-body fd) fds) fds))] 
    [(idC _) (error 'interp "AAQZ3 shouldn't get here got ~a" a )]))
 


;;interp test cases
(check-equal? (interp n1 '()) 1)
(check-equal? (interp p1 '()) 3)
(check-equal? (interp m2 '()) 12)

(check-equal? (interp (ifleq0? (numC 2) (numC 1) (numC 3)) '()) 3)
(check-equal? (interp (ifleq0? (numC -2) (numC 1) (numC 3)) '()) 1)

(check-equal? (interp (ifleq0?
                       (binopC '- (numC 2) (numC 4)) (numC 1) (numC 3)) '()) 1)
(check-equal? (interp (ifleq0?
                       (binopC '- (numC 2) (numC 4)) (binopC '* (numC 2) (numC 4)) (numC 3)) '()) 8)
 
;;parser


(define (parse-prog [progs : Sexp]) : (Listof FundefC)
  (match progs
    ['() '()]
    [(cons prog rest)
     (check-duplicate-func (parse-fundef prog) (parse-prog rest))]))
     #;(match prog
       [(list 'def (? symbol? name) '() '=> body) (cons (FundefC (idC name) '() (parse body)) (parse-prog rest))]
       [(list 'def (? symbol? name) (list args ...) '=> body)
        (if (andmap symbol? args) (cons (FundefC (idC name) (map idC args) (parse body)) (parse-prog rest))
             (error 'parse "AAQZ3 Expected a list of symbols for arguments"))]
       [_ (error "AAQZ3 Expected 'def' with correct syntax but found something else")])  
    ;;[_ (error "AAQZ3 Invalid program format")]
 
;;takes in a function and a list of function and check if its a repeated name

(define (check-duplicate-func [new : FundefC] [existing : (Listof FundefC)]) : (Listof FundefC)
  (match existing
    ['() (cons new existing)]
    [(cons func rest)
     (if (equal? (FundefC-name new) (FundefC-name func))
         (error "AAQZ3 found a syntax error repeated function name\n") (cons func (check-duplicate-func new rest)))]))



 

(define (parse-fundef [prog : Sexp]) : FundefC
  (match prog
    #;[(list 'def (? symbol? name) (list '() '=> body)) (FundefC (idC name) '() (parse body))]
    [(list 'def (? symbol? name) (list (list args ...) '=> body)) ;;make sure name is valid id
     (cond
       [(hash-has-key? invalid-table name) (error 'parse "Invalid identifier in AAQZ in parse-fundef: ~a" prog)]
       [(not (andmap symbol? args)) (error 'parse "AAQZ Expected a list of symbols for arguments got ~a" args)]
       [else (FundefC (idC name) (check-duplicate-arg (map idC args)) (parse body))])]
    [other (error 'parse-fundef "AAQZ found awrong function structure ~a" other)]))

 

(define (check-duplicate-arg [args : (Listof idC)]) : (Listof idC)
  (match args
    ['() '()]
    [(cons first rest) (cons (check-duplicate-arg-helper first rest) (check-duplicate-arg rest))]))

(define (check-duplicate-arg-helper [new : idC] [existing : (Listof idC)]) : idC
  (match existing
    ['() new]
    [(cons arg rest)
     (if (equal? new arg)
         (error "AAQZ3 found a syntax error repeated argument name\n") (check-duplicate-arg-helper new rest))]))
;;
#;(define (op-in-table? [op : Symbol]) : Boolean
  (hash-has-key? op-table op))

(define (parse [prog : Sexp]) : ExprC 
  (match prog
    [(? real? n) (numC n)]
    [(list 'ifleq0? test then else) (ifleq0? (parse test) (parse then) (parse else))]
    [(list (? symbol? op) l r)
     (if (hash-has-key? op-table op)
         (binopC op (parse l) (parse r))
         ;(error 'parse "Unsupported operator in AAQZ3: ~a" op)
         (if (hash-has-key? invalid-table op)
             (error 'parse "Invalid identifier: ~a in AAQZ3" prog)
             (appC (idC op) (list (parse l) (parse r)))))] ;;make sure op is valid id 
    [(list (? symbol? s) args ...)
     (if (hash-has-key? invalid-table s)
         (error 'parse "Invalid identifier: ~a in AAQZ3" prog)
         (appC (idC s) (map parse args)))]
    [(? symbol? s)
     (if (hash-has-key? invalid-table s)
         (error 'parse "Invalid identifier: ~a in AAQZ3" prog)
         (idC s))]
    [other (error 'parse "syntax error in AAQZ3, got ~e" other)])) ;; when do we reach here now?


;;parse test cases 

(check-equal? (parse '5)
             (numC 5))
              

(check-equal? (parse '{* {+ 2 3} 5})
              (binopC '*
                     (binopC '+ (numC 2) (numC 3))
                     (numC 5)))
;(check-exn #rx"Unsupported operator" (lambda () (parse '{> 3 4})))

(check-exn #rx"parse: Invalid identifier" (lambda () (parse '{+ 3})))

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

(check-equal? (interp (parse
                      '(sum3 (sum3 2 1 3) (addOne 1) 3))
                      (list (FundefC (idC 'addOne) (list (idC 'y)) (binopC '+ (idC 'y) (numC 1)))
                            (FundefC (idC 'sum3) (list (idC 'num) (idC 'num1) (idC 'num2))
                                     {binopC '+ {idC 'num2} {binopC '+ {idC 'num1} {idC 'num}}}))) 11) 




(check-equal? (interp-fns
       (parse-prog '{{def f2 {(x y) => {* x y}}}
                     {def f1 {(x y z a b) => {- {f2 {+ x y} z} {+ a b}}}}
                     {def f3 {() => 5}}
                     {def main {() => {+ {f1 1 2 3 4 {f2 (f3) (f3)}} {f2 2 (f3)}}}}})) 
      -10)

(check-exn #rx"Number of variables and arguments do not match"
           (lambda () (zip (list (numC 2) (numC 5)) (list (numC 8)))))
(check-exn #rx"AAQZ3 found an unbound variable:" (lambda () (subst-id (idC 'test) '() '())))
(check-exn #rx"reference to undefined function" (lambda () (get-fundef (idC 'test2) '())))
(check-exn #rx"interp: AAQZ3 shouldn't get here" (lambda () (interp (idC 'test3) '())))
(check-exn #rx"syntax error in AAQZ3, got" (lambda () (parse "Testing")))
(check-exn #rx"Invalid identifier:" (lambda () (parse '(def 2 3))))
 
(check-exn #rx"Invalid identifier in AAQZ in parse-fundef:" (lambda () (interp-fns
        (parse-prog '{{def * {(x y) => {+ x y}}} 
                     {def main {() => {* 1 2}}}}))))

(check-exn #rx"find-main: AAQZ3C cant find main" (lambda () (interp-fns
        (parse-prog '{{def testing {(x y) => {+ x y}}} 
                     {def fake-main {() => {* 1 2}}}})))) 

(check-exn #rx"AAQZ Expected a list of symbols for arguments" (lambda () (interp-fns
        (parse-prog '{{def test {((numC 5) y) => {+ x y}}} 
                     {def main {() => {* 1 2}}}}))))

(check-exn #rx"AAQZ3 found a syntax error repeated argument name" (lambda () (interp-fns
        (parse-prog '{{def f1 {(x x) => {+ x y}}} 
                     {def main {() => {f1 1 2}}}}))))

(check-exn #rx"AAQZ3 found a syntax error repeated function name" (lambda () (interp-fns
        (parse-prog '{{def f {() => 5}}
                      {def f {() => 5}} 
                      {def main {() => {+ {f} {f}}}}}))))

(check-exn #rx"AAQZ3 found an unbound variable" (lambda () (interp-fns
        (parse-prog '{{def f1 {(x) => {+ x y}}} 
                     {def main {() => {f1 1}}}}))))

(check-exn #rx"Number of variables and arguments do not match AAQZ3" (lambda () (interp-fns
        (parse-prog '{{def f1 {(x) => {+ x y}}} 
                     {def main {() => {f1 1 2}}}}))))

(check-exn #rx"AAQZ found awrong function structure" (lambda () (interp-fns
        (parse-prog '{{def f1 {(x) => {+ x y}}} 
                     {def main {() => {f1 1 2} 2}}}))))

(check-equal? (top-interp '{{def f2 {(x y) => {* x y}}}
                     {def f1 {(x y z a b) => {- {f2 {+ x y} z} {+ a b}}}}
                     {def f3 {() => 5}}
                     {def main {() => {+ {f1 1 2 3 4 {f2 (f3) (f3)}} {f2 2 (f3)}}}}})
      -10)
(check-exn #rx"Invalid identifier:" (lambda () (parse 'ifleq0?)))

(check-equal? (top-interp (quote ((def main (() => (+ (f 13) (f 0)))) (def f ((qq) => (ifleq0? qq qq (+ qq 1))))))) 14)
