#lang racket
(require "../metapict.rkt")

; Examples from the MetaFont Book
(def heart
  (curve (pt 100 162) .. (pt 140 178) right .. (pt 195 125) down 
         .. (pt 100 0) (curl 0) .. up (pt 5 125) .. right (pt 60 178) .. (pt 100 162)))
(def fig-heart   
  (with-window (window -100 300 -100 300) (draw heart)))
(def heart-line ; used to interpolate from line to heart below
  (curve (pt 100 0) -- (pt 300 0) -- (pt 200 0) -- (pt 100 0) -- (pt 0 0)
         -- (pt -100 0) -- (pt 100 0)))
(def fig-line-to-heart ; shows interpolation between two curves of same length
  (with-window (window -100 300 -100 300) 
    (apply draw (for/list ([i (in-range 10)])
                  (intercurve (/ i 10) heart-line heart)))))
"Examples from The MetaFont Book"  
(list "Heart" fig-heart fig-line-to-heart)