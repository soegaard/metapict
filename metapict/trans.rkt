#lang racket/base
;;; Affine transformations are represented by
;;;     (struct trans (xx yx xy yy x0 y0) ...]
;;; The point (x,y) is transformed to:
;;;     xnew = xx*x + xy*y + x0
;;;     ynew = yx*x + yy*y + y0
;;; Think of an affine transformation as a linear transformation followed by a translation.

(require racket/contract/base)
(provide (struct-out trans)
         ; Predefined transformations
         identity rotated90 rotated180 rotated270 flipx flipy 
         pair ; match expander that matches pt, vec, list or vector
         ; TODO easy: make contracts for these:
         rotated rotatedd rotated-about rotated-aboutd trans?
         (contract-out [inverse (-> trans? trans?)]        ; inverse transformation
                       [trans->vector (-> trans? vector?)] ; convert to vector
                       ))

(require "def.rkt" "structs.rkt" "trig.rkt" "mat.rkt" "pt-vec.rkt"
         (for-syntax racket/base) racket/match racket/format racket/list racket/math)

(define (constructor o)
  (cond [(vec? o)  vec] 
        [(pt? o)   pt]
        [(list? o) list]
        [(vector? o) vector]
        [else (error)]))

(struct trans (xx yx xy yy x0 y0) ; affine transformation:
  #:transparent                   ;   xnew = xx*x + xy*y + x0
  #:property                      ;   ynew = yx*x + yy*y + y0
  prop:procedure ; make transformations applicable
  (λ (t . vs)
    (defm (trans xx yx xy yy x0 y0) t)
    (define (f v)
      (defm (or (vec x y) (pt x y) (list x y) (vector x y)) v)
      (defv (xnew ynew) (values (+ (* xx x) (* xy y) x0) (+ (* yx x) (* yy y) y0)))
      (apply (constructor v) (list xnew ynew)))
    (let loop ([vs (reverse vs)])
      (match vs
        [(list v) (match v
                    [(bez p0 p1 p2 p3)    (bez (f p0) (f p1) (f p2) (f p3))]
                    [(curve: c? bs)       (curve: c? (map t bs))]
                    [(or (? vec?) (? pt?) (list _ _) (vector _ _)) (f v)]
                    [(? trans? v)         (compose-trans t v)]
                    ; [(? open-path? v)     (transform-path t v f)]
                    ; [(? closed-path? v)   (transform-path t v f)] ; XXX
                    [_ (error 'trans (~a "Can't transform the value: " v))])]
        [(list vn vn- v ...) (loop (cons (vn- vn) v))]))))


(module+ test (require rackunit)
  (check-equal? ((trans 1 2 3 4 5 6) (pt 0 0))  (pt 5 6))
  (check-equal? ((trans 1 2 3 4 5 6) (pt 1 0))  (pt 6 8))
  (check-equal? ((trans 1 2 3 4 5 6) (pt 0 1))  (pt 8 10))
  (check-equal? ((trans 1 2 3 4 5 6) (vec 0 0)) (vec 5 6))
  (check-equal? ((trans 1 2 3 4 5 6) '(0 0))    '(5 6)))

; Predefined Transformations
(def identity   (trans  1  0  0  1 0 0))
(def rotated90  (trans  0  1 -1  0 0 0))
(def rotated180 (trans -1  0  0 -1 0 0))
(def rotated270 (trans  0 -1  1  0 0 0))
(def flipy      (trans  1  0  0 -1 0 0))
(def flipx      (trans -1  0  0  1 0 0))

(module+ test
  (check-equal? (identity   (pt 2 3)) (pt  2  3))
  (check-equal? (rotated90  (pt 2 3)) (pt -3  2))
  (check-equal? (rotated180 (pt 2 3)) (pt -2 -3))
  (check-equal? (rotated270 (pt 2 3)) (pt  3 -2))
  (check-equal? (flipy      (pt 2 3)) (pt  2 -3))
  (check-equal? (flipx      (pt 2 3)) (pt -2  3)))

(require (for-syntax syntax/parse))
(define-match-expander pair 
  (λ (stx) (syntax-parse stx [(_ a b) #'(or (pt a b) (vec a b) (list a b) (vector a b))])))

; Standard Transformations
(define-syntax (define-trans/1 stx)
  (syntax-parse stx
    [(_ name var trans-expr)
     #'(begin 
         (provide name) ; todo easy: use contract-out here
         (define name (match-lambda* [(list* (? real? var) args)
                                      (define t trans-expr)
                                      (if (empty? args) t (apply t args))])))]))
(define-syntax (define-trans/2 stx)
  (syntax-parse stx
    [(_ name a b trans-expr)
     (syntax/loc stx
       (begin 
         (provide name) ; todo easy: use contract-out here
         (define name (match-lambda* [(or (list* (pair a b)              args) 
                                          (list* (? real? a) (? real? b) args))
                                      (define t trans-expr)
                                      (if (empty? args) t (apply t args))]))))]))

(define-trans/1 slanted a   (trans 1 0 a 1 0 0))     ; (x,y) slanted a     = (x+ay,y)
(define-trans/1 scaled  a   (trans a 0 0 a 0 0))     ; (x,y) scaled a      = (ax,ay)
(define-trans/1 xscaled a   (trans a 0 0 1 0 0))     ; (x,y) xscaled a     = (ax,y)
(define-trans/1 yscaled a   (trans 1 0 0 a 0 0))     ; (x,y) yscaled a     = (x,ay)
(define-trans/2 shifted a b (trans 1 0 0 1 a b))     ; (x,y) shifted (a,b) = (x+a,y+b)
(define-trans/2 zscaled a b (trans a b (- b) a 0 0)) ; (x,y) zscaled (a,b) = (ax-by,bx+ay)
; Note:  (1,0) zscaled (a,b) = (a,b) , thus (1,0) is rotated and scaled into (a,b)
;        Think of zscaled as "complex multiplaction"
(define-trans/1 rotated θ                       ; (x,y) rotated θ = (x cosθ - y sinθ, x sinθ + y cosθ)
  (let () (defv (cosθ sinθ) (cos&sin θ))
    (trans cosθ sinθ (- sinθ) cosθ 0 0)))
(define (rotatedd θ . args) (apply rotated (rad θ) args))
(define (rotated-about θ p . args)
  (defm (pt x y) p)
  (def t ((shifted x y) (rotated θ) (shifted (- x) (- y))))
  (if (empty? args) t (apply t args)))
(define (rotated-aboutd θ p . args) (apply rotated-about (rad θ) p args))
; (define (reflectedabout p q) (trans ...)) ; todo
; (define (rotatedaround p θ)  (trans ...)) ; todo


(define (det T) 
  (match T
    [(mat a b c d) (- (* a d) (* b c))]
    [(trans xx yx xy yy x0 y0) (- (* xx yy) (* xy yx))]))

(define (inverse t) ; ((inverse t) t) = identity
  (cond [(eq? t identity) t] ; fast identity
        [else
         (defm (trans xx yx xy yy x0 y0) t)
         (def A (mat xx xy yx yy))
         (when (zero? (det A)) (error 'inverse (~a "The transformation is not invertible: " t)))
         (defm A- (mat-inv A))
         (defm (vec x0* y0*) (mat*vec A- (vec x0 y0)))
         (defm (mat a b c d) A-)
         (trans a c b d (- x0*) (- y0*))]))

(define (trans~ t s [ε 1e-15]) 
  (defv (v w) (values (trans->vector t) (trans->vector s)))
  (def norm^2 (for/sum ([x (in-vector v)][y (in-vector w)]) (sqr (- x y))))
  (<= norm^2 (sqr ε)))

(module+ test
  (check-equal? ((shifted 1 2) (vec 4 5)) (vec 5 7))
  (check-equal? ((slanted 1)   (vec 4 5)) (vec 9 5))
  (check-equal? ((scaled 2)    (vec 4 5)) (vec 8 10))
  (check-equal? ((xscaled 2)   (vec 4 5)) (vec 8 5))
  (check-equal? ((yscaled 2)   (vec 4 5)) (vec 4 10))
  (check-equal? ((zscaled 2 3) (vec 1 0)) (vec 2 3))
  (check-true   (vec~ ((rotated π/2) (vec 1 0)) (vec 0 1)))
  (check-true   (vec~ ((rotated π/2) (vec 0 1)) (vec -1 0)))
  (check-true   (trans~ (inverse (rotated π/2)) (rotated -π/2)))
  (check-true   (trans~ (inverse (shifted 1 2)) (shifted -1 -2)))
  (check-true   (trans~ (inverse (scaled 3))    (scaled 1/3)))
  (check-true   (let ([T ((xscaled 1/3) (rotated pi/4) (shifted -1 -2))])
                  (trans~ (T (inverse T)) identity)
                  (trans~ ((inverse T) T) identity))))

(define (compose-trans t1 t2)
  ; ((compose-trans t1 t2) v) = (t1 (t2 v))
  (defm (trans a d b e c f) t1)
  (defm (trans g j h k i l) t2)
  (trans (+ (* a g) (* b j))   (+ (* d g) (* e j))
         (+ (* a h) (* b k))   (+ (* d h) (* e k))
         (+ (* a i) (* b l) c) (+ (* d i) (* e l) f)))

(module+ test
  (check-equal? (compose-trans (shifted 1 2) (shifted 4 5)) (shifted 5 7))
  (check-equal? (compose-trans (shifted 1 2) (scaled 3))    (trans 3 0 0 3 1 2))
  (check-equal? ((shifted 1 2) (scaled 3))   (trans 3 0 0 3 1 2))
  (let () (def R (rotated π/4)) (def S (shifted 1 2)) (def T (compose-trans R S))
    (check-true (vec~ (R (S right)) (T right)))
    (check-true (vec~ (R (S up)) (T up)))))

(define (trans->vector t)
  (defm (trans xx yx xy yy x0 y0) t)
  (vector xx yx xy yy x0 y0))


