#lang typed/racket
(require typed/rackunit)

;;defining the AAQX3C language
(define-type AAQX3C (U numC binopC squareC funDefC idC appC))

(struct numC[(n : Real)] #:transparent)
(struct binopC[(op : Symbol) (l : AAQX3C) (r : AAQX3C)] #:transparent)
(struct squareC[(n : AAQX3C)] #:transparent)
(struct funDefC [(name : idC) (args : (Listof idC)) (body : AAQX3C)] #:transparent)
(struct idC [(name : Symbol)] #:transparent)
(struct appC [(name : idC) (args : (Listof AAQX3C))] #:transparent)
(struct ifleq0? [(test : numC) (then : AAQX3C) (else : AAQX3C)] #:transparent)

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
  ;;[(appC n a) (appC n (subst-single what for a))]
  [(appC n a) (numC (interp (appC n (change-args subs a fds)) fds))]
  [(binopC op l r) (binopC op (subst subs l fds)
                      (subst subs r fds))]))
 

(define (change-args [subs : (Listof (Listof AAQX3C))] [args : (Listof AAQX3C)] [fds : (Listof funDefC)]) : (Listof AAQX3C)
  (match args
    ['() '()]
    [(cons (? idC? id) r) (cons (subst-id id subs fds) (change-args subs r fds))]
    [(cons other r) (cons other (change-args subs r fds))]))




(define (zip [l1 : (Listof AAQX3C)] [l2 : (Listof AAQX3C)]) : (Listof (Listof AAQX3C))
  (cond
    [(not (equal? (length l1) (length l2))) (error 'zip "Number of variables and arguments do not match AAQX3")]
    [(empty? l1) '()]
    [else (cons (list (first l1) (first l2)) (zip (rest l1) (rest l2)))]))

(define (AAQX3C=? [arg1 : AAQX3C] [arg2 : AAQX3C]) : Boolean
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
  (match
    ['() (error 'get-fundef "reference to undefined function AAQZ: ~a" n)]
    [(cons f ) (cond
                   [(equal? n (funDefC-name (first fds))) (first fds)]
                   [else (get-fundef n (rest fds))])]))

;;interpret function
(define (interp [a : AAQX3C] [fds : (Listof funDefC)]) : Real
  (match a
    [(numC n) n]
    [(binopC op l r) ((hash-ref op-table op) (interp l fds) (interp r fds))]
    [(squareC n) (expt (interp n fds) 2)]
    [(appC f a) (local ([define fd (get-fundef f fds)])
              (interp (subst (zip a (funDefC-args fd))
                             (funDefC-body fd) fds) fds))] ;;passes in the whole function body?
    [(idC _) (error 'interp "AAQX3 shouldn't get here")]))



;;interp test cases
(check-equal? (interp n1 '()) 1)
(check-equal? (interp p1 '()) 3)
(check-equal? (interp m2 '()) 12)
(check-equal? (interp s1 '()) 4)
   
;;parser 

(define (parse-prog [progs : Sexp]) : (Listof funDefC)
  (match progs
    ['() '()]
    [(cons prog rest)
     (match prog
       [(list 'def (? symbol? name) '() '=> body) (cons (funDefC (idC name) '() (parse body)) (parse-prog rest))]
       [(list 'def (? symbol? name) (list args ...) '=> body)
        (if (andmap symbol? args) (cons (funDefC (idC name) (map idC args) (parse body)) (parse-prog rest))
             (error 'parse "AAQX3 Expected a list of symbols for arguments"))]
       [_ (error "AAQX3 Expected 'def' with correct syntax but found something else")])]  
    [_ (error "AAQX3 Invalid program format")])) 

;;
(define (op-in-table? [op : Symbol]) : Boolean
  (hash-has-key? op-table op))

(define (parse [prog : Sexp]) : AAQX3C
  (match prog
    [(? real? n) (numC n)]
    [(list '^2 n) (squareC (parse n))]
    [(? symbol? s) (idC s)]
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


(check-equal? (parse-prog '{{def addOne (y) => 
                                     (+ y 1)}}) 
              (list (funDefC (idC 'addOne) (list (idC 'y)) (binopC '+ (idC 'y) (numC 1)))))

(check-equal? (parse-prog '{{def oneAddOne () =>
                                     (+ 1 1)}})
              (list (funDefC (idC 'oneAddOne) '() (binopC '+ (numC '1) (numC 1)))))

(check-equal? (parse-prog '{{def area (w h) =>
                                       (* w h)} })
                     (list (funDefC (idC 'area) (list (idC 'w) (idC 'h)) (binopC '* (idC 'w) (idC 'h))))) 

(check-equal? (parse-prog '{{def addOne (y) =>
                                     (+ y 1)} })
              (list (funDefC (idC 'addOne) (list (idC 'y)) (binopC '+ (idC 'y) (numC 1)))))

(check-equal? (interp (parse '(addOne 2)) (list (funDefC (idC 'addOne) (list (idC 'y)) (binopC '+ (idC 'y) (numC 1))))) 3)


(check-equal? (interp (parse
                      '(addFunction 2 addOne))
                      (list (funDefC (idC 'addOne) (list (idC 'y)) (binopC '+ (idC 'y) (numC 1)))
                            (funDefC (idC 'addFunction) (list (idC 'num) (idC 'callFunc)) {binopC '+ {idC 'num} {appC {idC 'callFunc} {list {idC 'num}}}}))) 5)

 





