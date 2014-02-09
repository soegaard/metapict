#lang racket
(require metapict)
(def p1 (pt -7 1.5))    ; left  vanishing point
(def p2 (pt  8 1.5))    ; right vanishing point
(def a1 (pt  0  0))     ; central top point
(def a2 (pt  0 -2))     ; central bottom point
(def a3 (med .8 p1 a2)) ; left bottom
(def a4 (med .8 p1 a1)) ; left top
(def a7 (med .7 p2 a2)) ; right bottom
(def a8 (med .7 p2 a1)) ; right top
(def a5 (intersection-point (curve a8 -- p1) (curve a4 -- p2)))
(def a6 (intersection-point (curve a7 -- p1) (curve a3 -- p2)))
(def f6 (curve a2 -- a3 -- a6 -- a7 -- cycle)) 
(def f3 (curve a3 -- a4 -- a5 -- a6 -- cycle))
(def f4 (curve a5 -- a6 -- a7 -- a8 -- cycle))

(set-curve-pict-size 300 240)
(with-window (window -2 3 -2.5 1.5)
  (draw (for/draw ([f (list f3 f4 f6)]
                   [c (map (λ (x) (color* x "gray")) '(.9 .7 .6))])
          (color c (fill f)))
        (penwidth 2
          (draw (curve a5 -- a6) (curve a3 -- a6) (curve a7 -- a6) ; back  lines
                (curve a1 -- a2) (curve a3 -- a4) (curve a7 -- a8) ; front lines
                (curve a1 -- a4) (curve a1 -- a8) (curve a2 -- a3) 
                (curve a2 -- a7) (curve a4 -- a5) (curve a8 -- a5)))
        (penwidth 8
          (color "red" (draw a1 a2 a3 a4 a5 a6 a7 a8)))))
