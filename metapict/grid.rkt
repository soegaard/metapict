#lang racket
(provide grid)


(require "def.rkt" "pt-vec.rkt" "structs.rkt" "draw.rkt" "curve.rkt" "path.rkt")

(define (grid lower-left upper-right reference step)
  ; Draw a grid with the rectangle given by the lower-left 
  ; and upper-right corner. Reference is a point in
  ; which the horizontal and vertical lines intersection.
  ; step is the distance between lines.
  (defm (pt x0 y0) lower-left)
  (defm (pt x1 y1) upper-right)
  (defm (pt rx ry) reference)
  (draw* (append (for/list ([x (in-range rx (+ x1 step) step)])
                   (curve (pt x y0) -- (pt x y1)))
                 (for/list ([x (in-range rx x0 (- step))])
                   (curve (pt x y0) -- (pt x y1)))
                 (for/list ([y (in-range ry (+ y1 step) step)])
                   (curve (pt x0 y) -- (pt x1 y)))
                 (for/list ([y (in-range ry y0 (- step))])
                   (curve (pt x0 y) -- (pt x1 y))))))