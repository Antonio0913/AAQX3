#lang typed/racket
(require typed/rackunit)
;;Progress Statement: We finished all of the parts of assignment three.
;;We first started by pasting in some of the functions from the lab 3.
;;We created the binopC struct first and used a hash table to map the
;;symbols to its corresponding operation. We then extended the given
;;parser and interpreter to accomodate this change. Function definitions,
;;identifiers, and function applications. This was a pretty big change and we
;;had some errors at first with this. We first started with the parser to accomodate
;;these new parts of our AAQZ3 language. Then we started to work on the interpreter.
;;The interpreter was difficult since we needed to find some way to substitute the arguments.
;;The substitution took up the majority of our time. We had to debug a few issues with our
;;implementation of substitution. We created helper functions for our main substitution function
;;to help account for all cases where we would need to substitute. After this addition,
;;most of the work was accounting for some unique cases such as division by 0, duplicate
;;function names, invalid identifiers, and duplicate argument names. We created multiple
;;helper functions. While accounting for edge cases, we realized we forgot to implement ifleq0?
;;so we implemented that right away. We kept testing and fixed all the errors that we encountered.


;;defining the AAQZ3 language
(define-type ExprC (U numC binopC FundefC idC appC ifleq0?))

(struct numC[(n : Real)] #:transparent)
(struct binopC[(op : Symbol) (l : ExprC) (r : ExprC)] #:transparent)
(struct FundefC [(name : idC) (args : (Listof idC)) (body : ExprC)] #:transparent)
(struct idC [(name : Symbol)] #:transparent)
(struct appC [(name : idC) (args : (Listof ExprC))] #:transparent)
(struct ifleq0? [(test : ExprC) (then : ExprC) (else : ExprC)] #:transparent)

;;defining hash table for operations
(define op-table
  (hash
    '+ +
    '* *
    '/ /
    '- -))

;;defining hash table for invalid identifier.
;;These symbols cannot be made into functions because we use these in our language
(define invalid-table
  (hash
   '+ 0
   '* 0
   '/ 0
   '- 0
   'def 0 
   'ifleq0? 0
   '=> 0))

;;takes in an S-expression, parses it, then calls the interp-fns function to interpret it.
(define (top-interp [funcs : Sexp])
  (interp-fns (parse-prog funcs)))

;;Takes a list of function, uses the find-main helper function to find the main function definition,
;;then interprets the body of main using the interp function.
(define (interp-fns [funs : (Listof FundefC)]) : Real
  (interp (FundefC-body (find-main funs)) funs))

;;Takes in an ExprC and a list of function definitions and converts the ExprC into a Real number.
;;The list of function defn is used to match function applications to their corresponding definitions.
(define (interp [a : ExprC] [fds : (Listof FundefC)]) : Real 
  (match a
    [(numC n) n]
    [(binopC op l r)
     (define right-val (interp r fds))
       (cond
         [(and (eq? op '/) (= right-val 0)) 
          (error 'interp "Division by zero at runtime in AAQZ!")] ;; Check if dividing by zero
         [else 
          ((hash-ref op-table op) (interp l fds) right-val)])]
    [(ifleq0? test then else)
     (if (<= (interp test fds) 0)
         (interp then fds)
         (interp else fds))]
    [(appC f a) (local ([define fd (get-fundef f fds)])
              (interp (subst (zip (interp-args a fds) (FundefC-args fd))
                             (FundefC-body fd) fds) fds))]
    [(idC _) (error 'interp "AAQZ3 shouldn't get here got ~a" a )]))

;;takes in an S-expression and parses it into our AAQZ3 language in the form of an ExprC.
;;Checks for invalid syntaxes and invalid identifiers.
(define (parse [prog : Sexp]) : ExprC 
  (match prog
    [(? real? n) (numC n)]
    [(list 'ifleq0? test then else) (ifleq0? (parse test) (parse then) (parse else))]
    [(list (? symbol? op) l r)
     (if (hash-has-key? op-table op)
         (binopC op (parse l) (parse r))
         (if (hash-has-key? invalid-table op) ;;make sure op is valid id 
             (error 'parse "Invalid identifier: ~a in AAQZ3" prog)
             (appC (idC op) (list (parse l) (parse r)))))] 
    [(list (? symbol? s) args ...)
     (if (hash-has-key? invalid-table s)
         (error 'parse "Invalid identifier: ~a in AAQZ3" prog)
         (appC (idC s) (map parse args)))]
    [(? symbol? s)
     (if (hash-has-key? invalid-table s)
         (error 'parse "Invalid identifier: ~a in AAQZ3" prog)
         (idC s))]
    [other (error 'parse "syntax error in AAQZ3, got ~e" other)]))

;;Takes in an S-expression representing a function definition, parses it, and returns a FundefC
;;checks for duplicate arguments using check-duplicate-arg, checks for invalid identifiers.
(define (parse-fundef [prog : Sexp]) : FundefC
  (match prog
    [(list 'def (? symbol? name) (list (list args ...) '=> body)) ;;make sure name is valid id
     (cond
       [(hash-has-key? invalid-table name) (error 'parse "Invalid identifier in AAQZ in parse-fundef: ~a" prog)]
       [(not (andmap symbol? args)) (error 'parse "AAQZ Expected a list of symbols for arguments got ~a" args)]
       [else (FundefC (idC name) (check-duplicate-arg (map idC args)) (parse body))])]
    [other (error 'parse-fundef "AAQZ found a wrong function structure ~a" other)]))

;;Takes in an S-expression representing a program, parses and returns a list of function definitions.
;;Checks for duplicate function names.
(define (parse-prog [progs : Sexp]) : (Listof FundefC)
  (match progs
    ['() '()]
    [(cons prog rest)
     (check-duplicate-func (parse-fundef prog) (parse-prog rest))]))

;;takes in a list of list of ExprC representing a list of substitutions,
;;an ExprC 'in', and a list of function definitions.
;;Recursively substitutes identifiers in the ExprC 'in' with corresponding expression from the list of substitutions.
;;returns the ExprC with all the substitutions completed.
(define (subst [subs : (Listof (Listof ExprC))] [in : ExprC] [fds : (Listof FundefC)]) : ExprC
  (match in
  [(numC n) in]
  [(idC s) (subst-id in subs)]
  [(binopC op l r) (binopC op (subst subs l fds)
                      (subst subs r fds))]
  [(appC n a) (appC n (change-args subs a fds))]
  [(ifleq0? con f1 f2) (ifleq0? (subst subs con fds) (subst subs f1 fds) (subst subs f2 fds))]))

;;Takes in an idC 's' and a list of list of ExprC containing an expression and its corresponding idC.
;;If the given idC 's' is in the list, return the corresponding expression. If its not found, throw an error
(define (subst-id [s : idC] [subs : (Listof (Listof ExprC))]) : ExprC
  (match subs
    ['() (error 'subst-id (format "AAQZ3 found an unbound variable: ~a" s))]
    [(cons (list what (? idC? for)) rest)
     (if (equal? s for)
         what
         (subst-id s rest))]))
    ;;[(cons _ rest) (subst-id s rest fds)]))

;;takes in a function and a list of function definitions and throws an error if there is a repeat function name.
;;Else adds the new function into the list of function definitions
(define (check-duplicate-func [new : FundefC] [existing : (Listof FundefC)]) : (Listof FundefC)
  (match existing
    ['() (cons new existing)]
    [(cons func rest)
     (if (equal? (FundefC-name new) (FundefC-name func))
         (error "AAQZ3 found a syntax error repeated function name\n")
         (cons func (check-duplicate-func new rest)))]))
 
;;takes in a list of idC representing arguments and checks if there are any duplicate names for
;;arguments in the given list using check-duplicate-arg-helper. Returns the list of arguments.
(define (check-duplicate-arg [args : (Listof idC)]) : (Listof idC)
  (match args
    ['() '()]
    [(cons first rest) (cons (check-duplicate-arg-helper first rest) (check-duplicate-arg rest))]))

;;Takes in an idC called 'new' and a list of idC and checks whether 'new' is in the list of idC. Throws
;;an error if new is found in the list of idC.
(define (check-duplicate-arg-helper [new : idC] [existing : (Listof idC)]) : idC
  (match existing
    ['() new]
    [(cons arg rest)
     (if (equal? new arg)
         (error "AAQZ3 found a syntax error repeated argument name\n")
         (check-duplicate-arg-helper new rest))]))

;;Takes in a Listof Listof ExprC representing a zipped list containing
;;function application values with their variable name, a Listof ExprC representing the args,
;;and a Listof FuncdefC representing all the functions parsed.
 (define (change-args [subs : (Listof (Listof ExprC))] [args : (Listof ExprC)]
                      [fds : (Listof FundefC)]) : (Listof ExprC)
   (match args 
    ['() '()]
    [(cons (? idC? id) r)
     (cons (subst-id id subs) (change-args subs r fds))]
    [(cons other r)
     (cons (subst subs other fds) (change-args subs r fds))]))

;;Takes in a list of ExprC representing function arguments and a list of function definitions (fds).
;;Recursively evaluates each argument, leaving numeric constants (numC) unchanged
;;and calling interp other expressions. Returns a list of evaluated ExprC.
(define (interp-args [args : (Listof ExprC)] [fds : (Listof FundefC)]) : (Listof ExprC)
  (match args
    ['() '()]
    [(cons (? numC? a) r) (cons a (interp-args r fds))]
    [(cons other r) (cons (numC (interp other fds)) (interp-args r fds))]))

;;Takes in two lists of ExprC with the same length and returns a List of List of ExprC
;;with each element containing the corresponding elements in the input list.
;;If the lists are not equal in length, return an error.
(define (zip [l1 : (Listof ExprC)] [l2 : (Listof ExprC)]) : (Listof (Listof ExprC))
  (match (list l1 l2)
    [(list '() '()) '()]
    [(list (cons f1 r1) (cons f2 r2)) (cons (list f1 f2) (zip r1 r2))]
    [other (error 'zip "Number of variables and arguments do not match AAQZ3: ~a" other)]))

;;Takes in an idC 'n' and a list of function definitions (fds), and returns the function definition
;;whose name matches the idC 'n'. If no match is found, it throws an error.
(define (get-fundef [n : idC] [fds : (Listof FundefC)]) : FundefC
  (match fds
    ['() (error 'get-fundef "reference to undefined function AAQZ: ~a" n)]
    [(cons first-fd rest-fds) (cond
                   [(equal? n (FundefC-name first-fd)) first-fd]
                   [else (get-fundef n rest-fds)])]))

;;Takes in a list of function definitions and returns the 'main' function definition. Throws
;;an error if no main function is found.
(define (find-main [funs : (Listof FundefC)]) : FundefC
  (match funs
    ['() (error 'find-main "AAQZ3C cant find main :(")]
    [(cons fun rest)
     (if (equal? (FundefC-name fun) (idC 'main)) fun (find-main rest))]))


;;-----TEST CASES!-----
;;interp test cases
(define n1 (numC 1))
(define n2 (numC 2))
(define n3 (numC 3))
(define n4 (numC 4))

(define p1 (binopC '+ n1 n2))
(define p2 (binopC '+ n3 n4))

(define m1 (binopC '* n1 n2))
(define m2 (binopC '* n3 n4))

(check-equal? (interp n1 '()) 1)
(check-equal? (interp p1 '()) 3)
(check-equal? (interp m2 '()) 12)

(check-equal? (interp (ifleq0? (numC 2) (numC 1) (numC 3)) '()) 3)
(check-equal? (interp (ifleq0? (numC -2) (numC 1) (numC 3)) '()) 1)

(check-equal? (interp (ifleq0?
                       (binopC '- (numC 2) (numC 4)) (numC 1) (numC 3)) '()) 1)
(check-equal? (interp (ifleq0?
                       (binopC '- (numC 2) (numC 4)) (binopC '* (numC 2) (numC 4)) (numC 3)) '()) 8)

;;testing parser
(check-equal? (parse '5)
             (numC 5))
              

(check-equal? (parse '{* {+ 2 3} 5})
              (binopC '*
                     (binopC '+ (numC 2) (numC 3))
                     (numC 5)))

(check-equal? (parse '{ifleq0? 1 2 3})
              (ifleq0? (numC 1) (numC 2) (numC 3))) 

;;MORE TESTS
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

(check-equal? (top-interp '{{def f2 {(x y) => {* x y}}}
                     {def f1 {(x y z a b) => {- {f2 {+ x y} z} {+ a b}}}}
                     {def f3 {() => 5}}
                     {def main {() => {+ {f1 1 2 3 4 {f2 (f3) (f3)}} {f2 2 (f3)}}}}})
              -10)

(check-equal?
 (top-interp
  '{{def f4 {(x y z) => {+ {f2 x y} {f3}}}}  ;; f2 used for multiplication
    {def f5 {(x y) => {- {f2 x y} {f1 x y 3 4}}}}  ;; Binary operators with 2 args
    {def f6 {(a b c d) => {* {+ a b} {f4 c d {f3}}}}} ;; Binary operators strictly between two arguments
    {def f2 {(x y) => {* x y}}} ;; Only 2 args in f2
    {def f1 {(x y z a) => {- {f2 {+ x y} z} {+ a z}}}} ;; Only 4 parameters for f1
    {def f3 {() => 5}}  ;; Constant function returning 5
    {def f7 {(a b) => {ifleq0? {f1 a b 1 2} {f4 5 6 {f3}} {f5 a b}}}} ;; Conditional using binary operators
    {def main {() => 
      {+ {- {+ {f6 10 20 {f3} {f5 2 {f7 {f6 10 20 {f5 2 {f7 {f3} {f7 2 3}}} {f2 {f3} {f4 2 3 4}}} 4}}}  
         {f6 10 20 {f3} {f6 2 5 {f5 6 {f7 {f2 6 5} {f5 1 2}}}
                            {f7 {f6 10 20 {f5 2 {f7 {f3} {f7 2 3}}} {f2 {f3} {f4 2 3 4}}} 4}}}}
         {f6 10 20 {f3}
             {f5 2 {f7 {f6 10 20 {f5 2 {f7 {f3}
                                           {f7 6
                                               {f1 {f1 {f1 2 3 4 5} {f2 {f2 3 4} 3} {f4 {f3} {f4 2 3 4} 3} 3} 3 3 4}}}}
                           {f2 {f3} {f4 2 3 4}}} 4}}}}
         {f6 10 20 {f3} {f5 2 {f7 {f6 10 20 {f5 2 {f7 {f3} {f7 2 3}}} {f2 {f3} {f4 2 3 4}}} 4}}}}}}}) -9554550)

(check-equal? (top-interp (quote ((def main (() => (+ (f 13) (f 0)))) (def f ((qq) => (ifleq0? qq qq (+ qq 1))))))) 14)

;;Testing error statements
(check-exn #rx"parse: Invalid identifier" (lambda () (parse '{+ 3})))

(check-exn #rx"Number of variables and arguments do not match"
           (lambda () (zip (list (numC 2) (numC 5)) (list (numC 8)))))

(check-exn #rx"AAQZ3 found an unbound variable:"
           (lambda () (subst-id (idC 'test) '())))

(check-exn #rx"reference to undefined function"
           (lambda () (get-fundef (idC 'test2) '())))

(check-exn #rx"interp: AAQZ3 shouldn't get here"
           (lambda () (interp (idC 'test3) '())))

(check-exn #rx"syntax error in AAQZ3, got"
           (lambda () (parse "Testing")))

(check-exn #rx"Invalid identifier:"
           (lambda () (parse '(def 2 3))))
 
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

(check-exn #rx"AAQZ found a wrong function structure" (lambda () (interp-fns
        (parse-prog '{{def f1 {(x) => {+ x y}}} 
                     {def main {() => {f1 1 2} 2}}}))))

(check-exn #rx"Invalid identifier:" (lambda () (parse 'ifleq0?)))


(check-exn #rx"interp: Division by zero at runtime in AAQZ!"
           (lambda ()
             (top-interp '((def ignoreit ((x) => (/ x 1)))
                           (def main (() => (/ (ignoreit (/ 1 0)) 0)))))))
(check-exn #rx"interp: Division by zero at runtime in AAQZ!"
           (lambda ()
             (top-interp '((def ignoreit ((x) => (/ 1 1)))
                           (def main (() => (/ (ignoreit (/ 1 0)) 0)))))))


   

