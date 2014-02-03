#lang racket
(provide (all-defined-out))

(require "structs.rkt" "def.rkt" "trig.rkt")
(module+ test (require rackunit))

; Predefined points and vectors
(def origo (pt 0 0))
(defv (north south west east) (values (vec 0 1) (vec 0 -1) (vec -1 0) (vec 1 0)))
(defv (up down left right) (values north south west east))

(define pt-
  (case-lambda
    [(p)   (match* (p)   
             [((pt x y))           (vec (- x) (- y))])]
    [(p q) (match* (p q) 
             [((pt x y) (vec s t)) (pt  (- x s) (- y t))]
             [((pt x y) (pt  s t)) (vec (- x s) (- y t))])]))

(define pt+ 
  (case-lambda 
    [()    (pt 0 0)]
    [(p)   p]
    [(p v) (match* (p v) [((pt x y) (or (vec s t)(pt s t))) (pt(+ s x)(+ t y))])]
    [ps    (for/fold ([sum (first ps)]) ([p (in-list (rest ps))]) (pt+ sum p))]))

(define (pt* s p) (match p [(pt x y) (pt (* s x) (* s y))]))
(define (dist p q) (norm (pt- q p)))
(define (pt= p q)  (and (= (pt-x p)  (pt-x q))  (= (pt-y p)  (pt-y q))))
(define (pt~ v w [ε 1e-15]) (<= (norm (pt- v w)) ε))
(define (med α p q) (def pq (pt- q p)) (pt+ p (vec* α pq)))  ; mediate aka linear interpolation
(define (pt@ r θ) (pt+ (pt 0 0) (vec@ r θ))) ; from polar: radius r, angle θ
(define (pt@d r θ) (pt+ (pt 0 0) (vec@ r (rad θ)))) ; from polar: radius r, angle θ

(module+ test
  (check-equal? (pt+ (pt 1 2) (vec 3 4)) (pt 4 6))
  (check-equal? (pt+ (pt 1 2) (vec 3 4) (vec 5 6)) (pt 9 12))
  (check-equal? (pt- (pt 1 2) (vec 3 4)) (pt -2 -2))
  (check-equal? (pt- (pt 1 2) (pt 3 4))  (vec -2 -2))
  (check-equal? (pt* 2 (pt 3 4)) (pt 6 8))
  (check-true   (pt= (pt 0 1) (pt 0 1)))
  (check-false  (pt= (pt 0 1) (pt 1 0)))
  (check-equal? (dist (pt 1 2) (pt 3 5)) (sqrt (+ (sqr (- 3 1)) (sqr (- 5 2)))))
  (check-equal? (med 1/2 (pt 1 2) (pt 3 6)) (pt 2 4))
  (check-equal? (med 1/3 (pt 1 2) (pt 4 8)) (pt 2 4))
  (check-equal? (pt@ 2 0) (pt 2 0))
  (check-true   (pt~ (pt@d 2 90) (pt 0 2)))
  (check-true   (pt~ (pt@ 2 pi) (pt -2 0))))

; Vectors (vecs)
(define (vec+ v w) (match* (v w) [((vec x y) (vec s t)) (vec (+ s x) (+ t y))]))
(define (vec- v w) (match* (v w) [((vec x y) (vec s t)) (vec (- x s) (- y t))]))
(define (vec* s v) (match v [(vec x y) (vec (* s x) (* s y))]))
(define (vec->pt v) (match v [(vec x y) (pt x y)]))
(define (vec= v w) (and (= (vec-x v) (vec-x w)) (= (vec-y v) (vec-y w))))
(define (vec~ v w [ε 1e-15]) (<= (norm (vec- v w)) ε))
(define (pos p) (vec (pt-x p) (pt-y p))) ; position vector (aka pt->vec)
(define (dot v w) (match* (v w) [((vec x y) (vec s t)) (+ (* x s) (* y t))])) ; inner product
(define (len2 v) (dot v v)) ; length squared
(define (len v) (sqrt (len2 v)))
(define norm len)
(define (dir/rad α) (vec (cos α) (sin α)))
(define (dir deg) (dir/rad (rad deg)))
(define (vec@ r θ) (vec* r (dir/rad θ))) ; from polar
(define (@ x) 
  ; these are from "angles.rkt", but used to prevent a module cycle
  (define (arccos x) ; this ensures a real result (rounding could lead to complex results)
    (acos (min 1.0 (max -1.0 x))))
  (define (angle2 v w) ; 0<= angle <= 2pi
    (def α (arccos (/ (dot v w) (* (len v) (len w)))))
    (cond [(negative? (dot w (rot90 v)))  (- (* 2. pi) α)] [α]))
  (define (angle v) ; 0<= angle <2pi ; directed angle from east to v, 
    (angle2 east v))
  (if (pt? x) (@ (pos x)) (values (len x) (angle x)))) ; to polar

(define (rot90 v)  
  (defm (or (vec x y) (pt x y)) v) 
  (def v^ (vec (- y) x))
  (if (vec? v) v^ (pt+ (pt 0 0) v^)))
(define (rot-90 v) 
  (defm (or (vec x y) (pt x y)) v) 
  (def v^^^ (vec  y (- x)))
  (if (vec? v) v^^^ (pt+ (pt 0 0) v^^^)))

(module+ test
  (check-equal? (vec+ (vec 1 2) (vec 3 4)) (vec 4 6))
  (check-equal? (vec- (vec 1 2) (vec 3 5)) (vec -2 -3))
  (check-equal? (vec* 2 (vec 3 4)) (vec 6 8))
  (check-equal? (vec->pt (vec 1 2)) (pt 1 2))
  (check-equal? (pos (pt 1 2)) (vec 1 2))
  (check-equal? (dot east east) 1)
  (check-equal? (dot north north) 1)
  (check-equal? (dot east north) 0)
  (check-equal? (len2 (vec 1 1)) 2)
  (check-equal? (len (vec 1 1)) (sqrt 2))
  (check-equal? (rot90 east) north)
  (check-equal? (rot90 north) west))
(module+ test
  (check-true (vec~ (dir   0) east))
  (check-true (vec~ (dir  90) north))
  (check-true (vec~ (dir 180) west))
  (check-true (vec~ (dir 270) south))
  (check-true (vec~ (dir 360) east)))
