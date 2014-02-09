#lang racket
;;; This module implements
;;;   i) angles between vecs (signed and unsigned)
;;;  ii) angle of vec measured from x-axis
;;; iii) angle reductions
;;;  iv) turning angle

; Intervals used in the contracts:
(define 0--2pi  real?)              ; [0;2pi]
(define 0-<2pi  real?)              ; [0;2pi[
(define -pi--pi real?)              ; ]-pi;pi]
(define -180--180 real?) 

(provide (contract-out 
          [angle2           (-> vec? vec?     0--2pi)] ;        angle (rad) between vecs
          [signed-angle2    (-> vec? vec?   -pi--pi)]  ; signed angle (rad) between vecs
          [angle            (-> vec?          0--2pi)] ;        angle (rad) from x-axis to vec
          [signed-angle     (-> vec?        -pi--pi)]  ; signed angle (rad) from x-axis to vec
          [reduce-angle     (-> deg?       -180--180)] ; add ±360 until result is in -180--180
          [reduce-angle/rad (-> deg?        -pi--pi)]  ; add ±2pi until result is in -pi--pi
          [turning-angle    (-> pt? pt? pt? -pi--pi)]  ; a polygonal line from p0 to p1 to p2 
          ;                                              turns left through this angle:
          )
         arccos)

(require "def.rkt" "structs.rkt" "pt-vec.rkt" "trig.rkt")
(module+ test (require rackunit))

(define (arccos x) ; this ensures a real result (rounding may lead to complex results)
  (acos (min 1.0 (max -1.0 x))))

(define (angle2 v w) ; 0<= angle <= 2pi
  (def α (arccos (/ (dot v w) (* (len v) (len w)))))
  (cond [(negative? (dot w (rot90 v)))  (- (* 2. pi) α)] [α]))
(define (angle v) ; 0<= angle <2pi ; directed angle from east to v, 
  (angle2 east v))
(define (signed-angle2 v w) ; -pi < angle <= pi
  (def α (arccos (/ (dot v w) (sqrt (* (len2 v) (len2 w))))))
  (cond [(< (dot w (rot90 v)) 0)  (- α)] [α]))
(define (signed-angle v ) ; -pi < angle <= pi
  (signed-angle2 east v))
(define (reduce-angle d)
  (def again reduce-angle)
  (if (> (abs d) 180)
      (if (> d 0) (again (- d 360)) (again (+ d 360)))
      d))
(define (reduce-angle/rad r)
  (def again reduce-angle/rad)
  (if (> (abs r) pi)
      (if (> r 0) (again (- r (* 2 pi))) (again (+ r (* 2 pi))))
      r))
(define (turning-angle p0 p1 p2)
  (signed-angle2 (pt- p1 p0) (pt- p2 p1)))

(module+ test
  (check-equal? (turning-angle (pt 0 0) (pt 1 0) (pt 2  0))  0.)
  (check-equal? (turning-angle (pt 0 0) (pt 1 0) (pt 1  1)) (rad 90))
  (check-equal? (turning-angle (pt 0 0) (pt 1 0) (pt 1 -1)) (rad -90)))

(module+ test
  (check-equal? (angle north) (rad 90))
  (check-equal? (angle2 east north) (rad 90))
  (check-equal? (signed-angle2 east north) (rad 90))
  (check-equal? (signed-angle2 east west) (rad 180))
  (check-equal? (signed-angle2 east south) (rad -90))
  (check-equal? (reduce-angle 90) 90)
  (check-equal? (reduce-angle (+ 90 360)) 90)
  (check-equal? (reduce-angle (- 90 360)) 90)
  (check-equal? (reduce-angle/rad (/ pi 4)) (/ pi 4))
  (check-equal? (reduce-angle/rad (+ (/ pi 4) (* 2 pi))) (/ pi 4))
  (check-equal? (reduce-angle/rad (- (/ pi 4) (* 2 pi))) (/ pi 4))
  (check-equal? (deg (angle2 east south)) 270.))
