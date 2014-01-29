#lang racket
;;; This module implements 2x2 matrices and rotations of pt and vec.
;;; Since matrices are for internal use, only mat*vec and mat-inv are defined.
;;; An 2x2 matrix is represented as: 
;;;   (struct mat (a b c d) #:transparent)

(provide (contract-out 
          [rot         (-> deg? vec?  vec?)] ; rotate vec the given degrees
          [rot/rad     (-> rad? vec?  vec?)] ; rotate vec the given radian
          [mat*vec     (-> mat? vec?  vec?)] ; multiply matrix and vector
          [mat-inv     (-> mat?       mat?)] ; inverse matrix          
          [rotmat      (-> deg?       mat?)] ; construct rotation matrix for the given degrees
          [rotmat/rad  (-> rad?       mat?)] ; construct rotation matrix for the given radians
          ))

(require "structs.rkt" "def.rkt" "trig.rkt" "pt-vec.rkt")        

(define (mat*vec A v)
  (match* (A v) 
    [((mat a b c d) (vec x y)) (vec (+ (* a x) (* b y)) (+ (* c x) (* d y)))]))

(define (mat-det A) (defm (mat a b c d) A) (- (* a d) (* b c)))

(define (mat-inv A) 
  (defm (mat a b c d) A) 
  (def D (- (* a d) (* b c)))
  (mat (/ d D)     (/ (- b) D) 
       (/ (- c) D) (/ a D)))

(module+ test (require rackunit))

(module+ test
  (def A (mat 1 2 3 4))
  (def A- (mat-inv A))
  (check-equal? (mat*vec A (mat*vec A- right)) right)
  (check-equal? (mat*vec A (mat*vec A- up)) up))


(define (rotmat θ) (rotmat/rad (rad θ)))

(define (rotmat/rad θ)
  (defv (cosθ sinθ) (cos&sin θ))
  (mat cosθ (- sinθ) sinθ cosθ))
(define (rot/rad θ v) (mat*vec (rotmat/rad θ) v))
(define (rot θ v) (rot/rad (rad θ) v))
(define (pt-rot θ p) (pt+ origo (rot θ (pt- p origo))))
(define (pt-rot/rad θ p) (pt+ origo (rot/rad θ (pt- p origo))))

(module+ test
  (check-true (vec~ (mat*vec (rotmat 90) east) north))
  (check-true (vec~ (mat*vec (rotmat 90) north) west)))
