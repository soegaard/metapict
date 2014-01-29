#lang racket
(require "../metapict.rkt")

(define (grid win stepx stepy)
  (defm (window xmin xmax ymin ymax) win)
  (def grid-xmin (ceiling (/ xmin stepx)))
  

(grid (window -5.1 5.1 6.1 6.0) 1 1)
