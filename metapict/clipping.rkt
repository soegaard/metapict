#lang racket/base
(require (for-syntax racket/base))

;;;
;;; Liang-Barski Line Clipping
;;;

;; See Wikipedia for an explanation of the algorithm

(provide window-clip)

(define (window-clip xmin xmax
                     ymin ymax
                     x1   y1
                     x2   y2)
  (define-syntax (inexact stx)
    (syntax-case stx ()
      [(_ id ...) #'(begin (set! id (if (exact? id) (exact->inexact id) id)) ...)]))
  (inexact xmin xmax ymin ymax x1 y1 x2 y2)
  
  (define  Δx (- x2 x1))
  (define  Δy (- y2 y1))
  (define -Δx (- Δx))
  (define -Δy (- Δy))

  (define p1 -Δx)
  (define p2  Δx)
  (define p3 -Δy)
  (define p4  Δy)

  (define q1 (- x1 xmin)) ; left
  (define q2 (- xmax x1)) ; right 
  (define q3 (- y1 ymin)) ; bottom
  (define q4 (- ymax y1)) ; top

  (cond
    [(or (and (= p1 0) (< q1 0))
         (and (= p2 0) (< q2 0))
         (and (= p3 0) (< q3 0))
         (and (= p4 0) (< q4 0)))
     ; line segment parallel to an edge, and outside window
     (list #f #f)]
    [else
     ; parameter values for intersection points
     (define r1 (and (not (= p1 0)) (/ q1 p1)))
     (define r2 (and (not (= p2 0)) (/ q2 p2)))
     (define r3 (and (not (= p3 0)) (/ q3 p3)))
     (define r4 (and (not (= p4 0)) (/ q4 p4)))
     ; candidates for parameter values of edge intersections
     (define u1 (max 0.
                     (if (negative? p1) r1 -inf.0)
                     (if (negative? p2) r2 -inf.0)
                     (if (negative? p3) r3 -inf.0)
                     (if (negative? p4) r4 -inf.0)))
     (define u2 (min 1.
                     (if (positive? p1) r1 +inf.0)
                     (if (positive? p2) r2 +inf.0)
                     (if (positive? p3) r3 +inf.0)
                     (if (positive? p4) r4 +inf.0)))
     (if (> u1 u2)
         ; line entirely outside window
         (list #f #f)
         ; points where line clips window
         (list (list (+ x1 (* u1 Δx)) (+ y1 (* u1 Δy)))
               (list (+ x1 (* u2 Δx)) (+ y1 (* u2 Δy)))))]))



;; ; horizontal line through window
;; (equal? (window-clip 0 2  0 2  -1 0  3 0) '((0. 0.) (2. 0.)))
;; ; horizontal line above window
;; (equal? (window-clip 0 2  0 2  -1 3  3 3) '(#f #f))
;; ; horizontal line below window
;; (equal? (window-clip 0 2  0 2  -1 -3  3 -3) '(#f #f))

;; ; vertical line through window
;; (equal? (window-clip 0 2  0 2  1 -1   1 3) '((1. 0.) (1. 2.)))
;; ; vertical line above window
;; (equal? (window-clip 0 2  0 2  3 -1   3 3) '(#f #f))
;; ; vertical line below window
;; (equal? (window-clip 0 2  0 2  -3 -1  -3 3) '(#f #f))


;; ; line through window
;; (equal? (window-clip 0 2  0 2  -1 0  2 3) '((0. 1.) (1. 2.)))
