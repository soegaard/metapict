#lang racket
(require metapict)

(define red    (curve (pt 0 0) -- (pt 8 0) -- (pt 8 3) -- cycle))
(define blue   (curve (pt 0 0) -- (pt 5 0) -- (pt 5 2) -- cycle))
(define green  (curve (pt 0 0) -- (pt 5 0) -- (pt 5 2) -- (pt 2 2) -- (pt 2 1) -- (pt 0 1) -- cycle))
(define yellow (curve (pt 0 0) -- (pt 2 0) -- (pt 2 1) -- (pt 5 1) -- (pt 5 2) -- (pt 0 2) -- cycle))

(define (draw-pieces positions)
  (for/draw ([p positions]
             [d (list  red   green   yellow   blue)]
             [c (list "red" "green" "yellow" "blue")])
    (def fill-color (change-alpha (color-med 0.2 c "magenta") 0.7))
    (def piece (shifted p d))
    (draw (color fill-color (fill piece))
          piece)))

(set-curve-pict-size 400 (* 13/15 400))
(with-window (window -1 14 -1 12)
  (define upper (list (pt 0 0) (pt 8 0) (pt 8 1) (pt 8 3)))
  (define lower (list (pt 5 2) (pt 8 0) (pt 5 0) (pt 0 0)))
  (margin 2 (draw (color "gray" (draw (grid (pt -1 -1) (pt 14 12) (pt 0 0) #:step 1)))
                  (draw-pieces (map (shifted (pt 0 6)) upper))
                  (draw-pieces lower))))
