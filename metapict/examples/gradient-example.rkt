#lang racket
(require metapict)
(set-curve-pict-size 300 300)

; Center and corners in triangle. Polar coordinates (radius,degrees)
(defv (O A B C) (values (pt 0 0) (pt@d 1 90) (pt@d 1 210) (pt@d 1 330)))

; Fill the three interior triangles using a 
; gradient parallel with the outer edge.
(with-window (window -1 1 -1 1) 
  (def (tri P Q . colors)
    (brushgradient P Q colors 
      (fill (curve P -- Q -- O -- cycle))))
  (draw (tri A B "yellow" "red")
        (tri B C "red"    "blue")
        (tri C A "blue"   "yellow")))

; Fill the triangle ABC thrice. The gradients are strongest
; at the corner. The direction is perpendicular to the side.
(with-window (window -1 1 -1 1)
  (def ABC (curve A -- B -- C -- cycle))
  (def (tri P Q c) (brushgradient P Q (list c (change-alpha c 0)) (fill ABC)))
  (draw (tri A (pt@d 1/2 (+  90 180)) "red")
        (tri B (pt@d 1/2 (+ 210 180)) "green")
        (tri C (pt@d 1/2 (- 330 180)) "blue")))
