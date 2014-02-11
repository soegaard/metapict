#lang racket
(require metapict)
;;; Hobby Control
;;; http://www.piprime.fr/966/official_asymptote_example-Hobbycontrol/

; The image illustrates this situation:

; A curve from p0 to p1 
;  - leaves p0 in direction ω0 
;  - enters p1 in direction ω1
;  - has angle θ from p0p1 to ω0
;  - has angle φ from p0p1 to ω1

(set-curve-pict-size 800 800)
(defv (p0 p1 p2) (values (pt 0 0) (pt 0.5 3) (pt 2 1)))
(def g (curve p0 .. p1 .. p2))
(def d0 (vec* 2/3 (direction-of g 0)))
(def d1 (vec* 1/2 (direction-of g 1)))

(with-window (window -1 6 -1 6)
  (draw (color "blue" 
               (draw (dashed (draw-arrow (curve (pt- p0 d0) .. (pt+ p0 d0))))
                     (label-top "w0" (pt+ p0 d0))
                     (dashed (draw-arrow (curve (pt- p1 d1) .. (pt+ p1 d1))))
                     (label-top "w1" (pt+ p1 d1))
                     (color "blue" (draw (subcurve g 0 1)))))
        (dashed (draw (curve p0 -- (med 1.25 p0 p1))))  ; p0p1 extended
        (color "red" 
               (draw (draw-arrow (shifted p0 (arc 0.4 (angle (pt- p1 p0)) (angle d0)))) ; theta
                     (draw-arrow (shifted p1 (arc 0.4 (angle (pt- p1 p0)) (angle d1)))) ; phi
                     (label-top "θ" (pt+ p0 (pt@d 0.4 125)))
                     (label-top "φ" (pt+ p1 (pt@d 0.4 45)))
                     (dot-label-llft "p0" p0)
                     (dot-label-lrt "p1" p1)))))
