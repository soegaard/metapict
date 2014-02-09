#lang racket
;;;
;;; Lines
;;;

; This module is currently not used.
; TODO: Use the line intersections in the computation of
; intersections of two Bezier curves in "bez.rkt".

(require "def.rkt" "structs.rkt" "window.rkt")

(struct line-equation (a b c) #:transparent) ; ax+by=c

(define (two-points->line-equation p0 p1)
  ; compute an equation for the line through p0 and p1
  (defm (pt x0 y0) p0)
  (defm (pt x1 y1) p1)
  (def a (- y1 y0))
  (def b (- x0 x1))
  (def c (+ (* a x0) (* b y0)))
  (line-equation a b c))

(define (line-intersection l0 l1)
  (defm (line-equation a0 b0 c0) l0)
  (defm (line-equation a1 b1 c1) l1)
  (def det (- (* a0 b1) (* a1 b0)))
  (cond [(zero? det) #f] ; lines are parallel 
        [else        (pt (/ (- (* b1 c0) (* b0 c1)) det)
                         (/ (- (* a0 c1) (* a1 c0)) det))]))

(define (line-segment-bounding-box p0 p1)
  (defm (pt x0 y0) p0) (defm (pt x1 y1) p1)
  (window (min x0 x1) (max x0 x1) (min y0 y1) (max y0 y1)))

(define (line-segment-intersection p0 p1 q0 q1)
  ; compute a possible intersectio between the line segments p0p1 and q0q1
  (def p (line-intersection (two-points->line-equation p0 p1) 
                            (two-points->line-equation q0 q1)))
  (and (pt-in-window? p (line-segment-bounding-box p0 p1))
       (pt-in-window? p (line-segment-bounding-box q0 q1))
       p))

(module+ test (require rackunit)
  (check-equal? (line-segment-intersection (pt 0 0) (pt 2 0) (pt 1 -1) (pt 1 1)) (pt 1 0))
  (check-equal? (line-segment-intersection (pt 0 0) (pt 2 2) (pt 0 2) (pt 2 0)) (pt 1 1))
  (check-equal? (line-segment-intersection (pt 2 2) (pt 3 3) (pt 0 2) (pt 2 0)) #f))