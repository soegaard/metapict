#lang racket
(require metapict)

(def r 0.75)                ; radius of the large circle with center midAB
(def A (pt 0 0))            
(def B (pt 1 0))            ; the diameter of the large circle is 1
(def C (med r A B))         ; C is between A and B

(def midAB (med 1/2 A B))   ; midpoint of A and B
(def midAC (med 1/2 A C))   ; midpoint of A and C
(def midBC (med 1/2 B C))   ; midpoint of B and C

(define (half-circle center radius)  
  ; make half-circle given center and radius
  (shifted center (arc radius 0 pi)))

(def h1 (half-circle midAB 1/2))           ; half circle, diameter 1
(def h2 (half-circle midAC (/ r 2)))       ; diameter r
(def h3 (half-circle midBC (/ (- 1 r) 2))) ; diamter  1-r

(set-curve-pict-size 300 150)
(with-window (window/aspect -0.1 1.2)
  (def red  (color-med 0.15 "red"  "teal")) ; mix in some teal to 
  (def blue (color-med 0.15 "blue" "teal")) ; make the colors more interesting
  (draw (pencolor "black" (brushcolor red  (filldraw h2)))
        (pencolor "black" (brushcolor blue (filldraw h3)))
        h1
        (curve A -- B)
        (label-bot "Arbelos: The white area equals the red and blue areas." midAB)))
