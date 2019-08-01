#lang racket/base
(provide grid)

(require "def.rkt" "pt-vec.rkt" "structs.rkt" "draw.rkt" "curve.rkt" "path.rkt")

(define (grid lower-left upper-right [reference origo] #:step [step 1])
  ; Draw a grid with the rectangle given by the lower-left 
  ; and upper-right corner. Reference is a point in
  ; which the horizontal and vertical lines intersect.
  ; step is the distance between lines.
  (defm (pt x0 y0) lower-left)
  (defm (pt x1 y1) upper-right)
  (defm (pt rx ry) reference)
  (define (end+ from to step) (if (integer? (/ (- to from) step)) (+ to step) to))
  (define (end- from to step) (if (integer? (/ (- from to) step)) (- to step) to))
  (draw* (append (for/list ([x (in-range rx (end+ rx x1 step) step)])
                   (curve (pt x y0) -- (pt x y1)))
                 (for/list ([x (in-range rx (end- rx x0 step) (- step))])
                   (curve (pt x y0) -- (pt x y1)))
                 (for/list ([y (in-range ry (end+ ry y1 step) step)])
                   (curve (pt x0 y) -- (pt x1 y)))
                 (for/list ([y (in-range ry (end- ry y0 step) (- step))])
                   (curve (pt x0 y) -- (pt x1 y))))))
