#lang racket
(require metapict)
(set-curve-pict-size 300 300)
(def col1 "red")
(def col2 "green")

(defv (p0 p1 p2) (values (pt 0 0) (pt -1 0) (pt 1 0)))
(def r 1.5)
(def c1 (circle p1 r))
(def c2 (circle p2 r))
(def p (pt 0 -2))
(def v (pt- p p0))
(def m 3)

(with-window (window -3 3 -3 3)
  (def intersection (color (color+ col1 col2) (clipped (fill c1) c2)))
  (draw (color col1 (fill c1))
        (color col2 (fill c2))
        intersection
        (label-top "A" p1)
        (label-top "B" p2)
        
        (label-bot "A∪B" p)
        (draw-arrow (curve p -- p0))
        (draw-arrow (curve p -- p1))
        (draw-arrow (curve p -- p2))
        
        (label-top "A∩B" (pt- p0 v))
        (draw-arrow (curve (pt- p0 v) -- (pt- p0 (vec* 0.2 v))))))
