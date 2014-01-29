#lang racket
(require "../def.rkt")
(module+ test (require rackunit "../def.rkt"))

; A linear expression is represented as an expr struct.
; The field coefs contains a hash table that maps variables to numbers (coeffecients).
; The "variable" 1 is mapped to the constant term.
(struct expr (coefs) #:transparent)
; Example:
(module+ test 
  (def 2x+y=7   (expr #hash((x . 2) (y . 1) (1 . -7))))
  (def 4x+2y=14 (expr #hash((x . 4) (y . 2) (1 . -14))))
  (def 6x+3y=21 (expr #hash((x . 6) (y . 3) (1 . -21))))
  (def 2x+2y=9  (expr #hash((x . 2) (y . 2) (1 . -9))))
  (def x        (expr #hash((x . 1) (1 . 0))))
  (def y        (expr #hash((y . 1) (1 . 0))))
  (def one      (expr #hash((1 . 1)))))

(define (variable? v) (symbol? v))
(define (variable-or-1? v) (or (variable? v) (equal? v 1)))

; expr-hash-ref : expr-hash variable -> number
;     return the coeffecient of the variable v
(define (expr-hash-ref h v) 
  (unless (variable-or-1? v) (error 'expr-hash-ref (~a "variable or 1 expected, got " v)))
  (hash-ref h v 0))

; ref : (or expr expr-hash) variable -> number
(define (ref e-or-h v)
  (match e-or-h
    [(expr h)    (expr-hash-ref h v)]
    [(? hash? h) (expr-hash-ref h v)]
    [_ (error 'ref "expr or hash expected")]))

(module+ test 
  (check-equal? (ref 2x+y=7 'x)  2) (check-equal? (ref 2x+y=7 'y) 1) 
  (check-equal? (ref 2x+y=7 '1) -7) (check-equal? (ref 2x+y=7 'z) 0)
  (check-equal? (ref #hash((x . 2)) 'x) 2))

; constant-term : expr -> number
;    return the constant term of the expression
(define (constant-term e)
  (ref e 1))

(module+ test 
  (check-equal? (constant-term 2x+y=7) -7)
  (check-equal? (constant-term x) 0))

; variables-in-equation : expr -> (setof variable)
;   return set of the variables in the expression
(define (variables-in-expr e)
  (defm (expr h) e)
  (set-remove (list->set (hash-keys h)) 1))

(module+ test (check-equal? (variables-in-expr 2x+y=7) (set 'x 'y)))

; variables-in-exprs : exprs -> (setof variable)
;    return set of all variables used in the expressions
(define (variables-in-exprs es)
  (apply set-union (map variables-in-expr es)))

(module+ test (check-equal? (variables-in-exprs (list 2x+y=7 x y)) (set 'x 'y)))

; expr= : expr expr -> boolean
;   do the exprs represent the same mathematical expression?
(define (expr= e1 e2)
  (defm (expr h) e1)
  (defm (expr k) e2)
  (and (= (hash-count h) (hash-count k))
       (for/and ([(v c) (in-hash h)])
         (= c (hash-ref k v)))))

; expr* : number expr -> expr
;   multiply all coeffecients in the expression e with k
(define (expr* k e)
  (defm (expr h) e)
  (match k
    [0  zero-expr]
    [_  (expr (for/hash ([(v c) (in-hash h)]) 
                (values v (* k c))))]))

(module+ test (check-true (expr= (expr* 3 2x+y=7) 6x+3y=21)))

; expr+ : expr expr -> expr
;   return an expr representing the sum of e1 and e2
(define (expr+ e1 e2)
  (defm (expr h) e1)
  (defm (expr k) e2)
  (def vs (append (hash-keys h) (hash-keys k)))
  (expr (for/hash ([v (in-list vs)]
                   #:unless (= (+ (ref h v) (ref k v)) 0))
          (values v (+ (ref h v) (ref k v))))))

(module+ test (check-true (expr= (expr+ 2x+y=7 2x+y=7) 4x+2y=14)))

; expr-lincomb : number expr number expr -> expr
;   compute the linear combination a*e1+b*e2
(define (expr-lincomb a e1 b e2)
  (expr+ (expr* a e1) (expr* b e2)))

(module+ test 
  (check-true (expr= (expr-lincomb 2 x 1 (expr-lincomb 1 y -7 one)) 2x+y=7)))

; expr~ : expr -> expr
;  negate the expression e
(define (expr~ e)
  (defm (expr h) e)
  (expr (for/hash ([(v c) h])
          (values v (- c)))))

(module+ test (check-true (expr= (expr~ 2x+y=7) (expr* -1 2x+y=7))))


; known	A table in which the keys are known variables and the values are expressions 
;          giving the values of those variables in terms of inputs and unknowns. 
;          value initially contains only inputs; the value of an input x is x.
(def known (make-parameter #hash()))

(define (register-known v c)
  (def k (known))
  (when (hash-has-key? k v) 
    (error 'register-known (~a "The variable " v " is already known.")))
  (known (hash-set k v c)))

(module+ test
  (parameterize ([known #hash()])
    (register-known 'x 42)
    (check-equal? (ref (known) 'x) 42))
  (check-equal? (ref (known) 'x) 0))

; fill-in-knowns : expr -> expr
;     eliminate the known variables from the expr e
(define (fill-in-knowns e)
  (defm (expr h) e)
  (def k (known))
  (define (known? v) (hash-has-key? k v))
  (def var-part   (for/list ([(v c) h] #:unless (or (equal? v 1) (known? v))) (cons v c)))
  (def const-part (+ (constant-term e)
                     (for/sum ([(v c) h] #:when (and (known? v) (not (equal? v 1))))
                       (* c (ref k v)))))
  (expr (make-hash `((1 . ,const-part) ,@var-part))))

(module+ test
  (parameterize ([known #hash((x . 2))])
    (expr= (fill-in-knowns 2x+y=7)
           (expr #hash((y . 1) (1 . -7))))))


; fill-in-one-known : expr variable number -> expr
;    substitute n for v in e
(define (fill-in-one-known e v n)
  (defm (expr h) e)
  (def c (ref h v))
  (def t (ref h 1))
  (expr (hash-set (hash-remove h v) 1 (+ t (* c n)))))

(module+ test
  (check-true (expr= (fill-in-one-known 2x+y=7 'x 3)
                     (expr #hash((y . 1) (1 . -1))))))


; non-constant-variables : expr -> (listof variable)
;    return list of variables
(define (non-constant-variables e)
  (defm (expr h) e)
  (for/list ([(v c) h] #:unless (equal? v 1)) v))

(module+ test (check-equal? (sort (non-constant-variables 2x+y=7) variable<) '(x y)))

;; A trivial equation is an equation with one non-constant variable.

; trivial? : expr -> boolean
;   is e a trivial expression? 
(define (trivial? e)
  (= 1 (length (non-constant-variables e))))

(module+ test
  (def 2x=7 (expr #hash((x . 2) (1 . -7))))
  (check-true (trivial? 2x=7))
  (check-false (trivial? 2x+y=7)))

; solve-trivial : expr -> (values variable number)
(define (solve-trivial e)
  (unless (trivial? e) (error 'solve-trivial (~a "trivial expr expected, got" e)))
  (defm (expr h) e)
  (def t (constant-term e))
  (def v (first (non-constant-variables e)))
  (def c (ref e v))
  (when (= c 0) (error 'solve-trivial (~a "coeffecient to " v "is zero")))
  (values v (/ (- t) c)))

(module+ test
  (let () (defv (x v) (solve-trivial 2x=7))
    (check-equal? (list x v) '(x 7/2))))


; zeroes	A list of expressions that are known to be zero. 
;          It is computed initially by subtracting the right-hand sides of the 
;          equations from the left-hand sides.
(def zeroes (make-parameter (list)))

(define (find-expr-with-max-coef v es)
  (argmax (λ(e) (ref e v)) es))

(define (reduce r v e)
  ; assume coef to v in r is 1
  (expr-lincomb 1 e (- (ref e v)) r))
  

(define (row-reduce exprs)
  (def vars (variables-in-exprs exprs))
  (for/fold ([used '()] [remaining exprs])  ([v (in-set vars)] #:break (empty? remaining))
    (def em (find-expr-with-max-coef v remaining))
    (let ([remaining (remove em remaining eq?)])
      (def e (expr* (/ 1 (ref em v)) em))
      (def (reduce/e expr) (reduce e v expr))
      (values (cons e (map reduce/e used))
              (map reduce/e remaining)))))

(module+ test
  (defv (done todo) (row-reduce (list 2x+y=7 2x+2y=9)))
  (displayln (list done todo)))

; solve : (listof expr) (hash (var . number)) -> ?
;   Inputs are known variables
; inputs:  The set of input variables.
#;(define (solve equations inputs)
    ; pending      A temporary list, holding zero-valued expressions from which no variable 
    ;              can be eliminated. These pending expressions are returned to zeroes at 
    ;              the end of each step of the solving algorithm.
    
    ; constraints  A list of constraints that must be satisfied if the equations are to 
    ;              have a solution. They come from equations in which all variables are 
    ;              known. The list is initially empty.
    ; invariants
    ; i) No variable in zeroes, pending, or constraints is dependent.
    ;    Note: i) => dependent vars are in value  
    ; ii) No variable appearing in an expression in value is dependent.
    ;    Note: Before putting expressions in value, eliminate the dependent variables
    ; iii) If all the variables on one side of a balance are known, then 
    ;      all the variables on the other side of that balance are known.
    ;    Note: Balances not in use in MetaPost/Font
    ; iv) Values in zeroes, pending, constraints, and value are all represented as sums of     
    ; linear terms.
    ;   Note: No problem!
    (define (solve-loop known zeroes pending)
      (match zeroes
        [(? empty?)   (match pending
                        [(? empty?) (make-all-dependent-variables-known)
                                    (build-and-return-solution)]
                        [_          (error 'solve-loop "underdetermined")])]
        [(list* z zs) (cond [(is-there-v-in-z-that-can-be-eliminated)
                             (eliminate-v-from-z) ...]
                            [(has-unknown-variable? z)
                             (solve-loop known zs (cons z pending))]
                            [else
                             ???])]))
    (solve-loop (known) (zeroes) '()))
#;(define (input->equation i)
    (expr 
     (make-hash
      (match (ref inputs i)
        [0 (list (cons i 1))]
        [k (list (cons i 1) (cons 1 (- k)))]))))
#;(solve-loop (map input->equation inputs) ; value
              equations                    ; zeroes
              '())                        ; pending



; (declare a)
; (== (* 2 a)  3)
; (== (+ (* 2 a) b) 21)
; (value-of a)

;;;
;;; Printing
;;;

(define (variable< s t)
  (match* (s t)
    [((? number? x) (? number? y)) (< x y)]
    [((? number? x) (? symbol? y)) #f]
    [((? symbol? x) (? number? y)) #t]
    [((? symbol? x) (? symbol? y)) (string<? (symbol->string x) (symbol->string y))]))

(module+ test
  (check-true  (variable< 'a 'b))
  (check-true  (variable< 'a  2))
  (check-true  (variable<  2  3))
  (check-false (variable<  2 'a)))

; expr->string : expr -> string
(define (expr->string e)
  (defm (expr h) e)
  (def vcs (for/list ([(v c) (in-hash h)] #:unless (= c 0)) (list v c)))  
  (def s (string-join (for/list ([vc (in-list (sort vcs variable< #:key first))])
                        (defm (list v c) vc)
                        (~a (match c 
                              [(? negative?) (~a "(" c ")")]
                              [1             (match v [1 1] [_ ""])]
                              [_             c])
                            (match v [1 ""] [_ v])))
                      "+"))
  (match s ["" "0"] [_ s]))

(module+ test 
  (check-equal? (expr->string (expr (make-hash '((x . 2) (y . 2)  (1 . 3))))) "2x+2y+3")
  (check-equal? (expr->string (expr (make-hash '((x . 2) (y . -2) (1 . 3))))) "2x+(-2)y+3"))

(define (display-expr e) 
  (display (expr->string e)))

(def zero-expr (expr (make-hash '((1 . 0)))))
(module+ test  (check-equal? (expr->string zero-expr) "0"))


(def eq1 (expr (make-hash '((x . 2) (y . 1) (1 . -7)))))    ;  2x+ y= 7
(def eq2 (expr (make-hash '((x . 1) (y . 3) (1 . -11)))))   ;  2x+3y=11

(expr->string eq1)
(expr->string (expr~ eq1))
(expr->string (expr~ (expr~ eq1)))
(expr= eq1 eq1)
(expr->string (expr* 2 eq1))
(expr->string (expr+ eq1 eq1))
(expr->string (expr+ eq1 (expr~ eq1)))


