#lang racket
(require "../metapict.rkt")

; Examples from the MetaPost Manual   mpman.pdf
(def (waves-clipped-in-circle) ; fig 41 p. 46
  (let ()
    (def p1 (curve (pt 0 0) (curl 0) .. (pt 5 -3) .. (curl 0) (pt 10 0)))
    (def p2 (curve-append p1 ((shifted 10 0) (yscaled -1) p1)))            ; 1 wave
    (def p0 p2)
    (for ([i (in-range 1 4)])
      (set! p0 (curve-append p0 ((shifted (* i 20) 0) p2))))
    (def waves
      (draw*
       (for/list ([j (in-range 0 9)])
         ((shifted 0 (* j 10)) p0))))
    (def p3 ((scaled 72) (shifted .5 .5) fullcircle))
    (draw (color "red" (draw p3))
          (clipped waves p3))))

#;(def fig-mpman-21
    (let ()
      (def p (curve (pt -1 0) .. (pt 0 -1) .. (pt 1 0)))
      (draw (fill (curve p up .. (pt 0 0) (vec -1 -2) .. up cycle))
            (curve p .. (pt 0 1) .. cycle))))


"Examples from The MetaPost Manual"
(list "Waves in circle" (with-scaled-window 70 (waves-clipped-in-circle)))