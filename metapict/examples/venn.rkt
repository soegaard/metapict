#lang racket
(require metapict)

(def x  10)
(def -x (- x))

(set-curve-pict-size 800 800)

(with-window (window -x x -x x)
  (def r 5)
  (def s (/ r 1.8))
  (def -r (- r))

  (def c1 (circle (pt (- s) 0) r))
  (def c2 (circle (pt    s 0)  r))
  (def rev curve-reverse)

  ; we split the drawing window into a left and right part
  (def left  (rectangle (pt -x -x) (pt 0 x)))
  (def right (rectangle (pt  0 -x) (pt x x)))

  ; colors
  (def red  (color-med 0.2 "red"  "black"))
  (def blue (color-med 0.2 "blue" "black"))
  (def mag  (color-med 0.5 "red"  "blue"))

  (def font (make-similar-font (new-font)
                               #:size 40
                               #:face "Arial"))
  (text-color "white"
    (with-font font
      (draw
       ; filled areas
       (brushcolor mag                 (fill c1      c2))
       (brushcolor red  (clipped left  (fill c1 (rev c2))))
       (brushcolor blue (clipped right (fill c2 (rev c1))))
       ; outlines
       c1 c2
       ; labels
       (label-cnt "A"     (pt -r 0))
       (label-cnt "B"     (pt  r 0))
       (label-cnt "A ∩ B" (pt  0 0))))))























