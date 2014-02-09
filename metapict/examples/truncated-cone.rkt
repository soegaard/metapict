#lang racket
(require metapict)
; http://www.texample.net/tikz/examples/truncated-cone/
(set-curve-pict-size 300 300)
(with-window (window -1 5 -1 5)
  (margin 5 (penwidth 2
    (draw ; (grid (pt -5 -5) (pt 5 5) (pt 0 0) 1)
          (dashed (color "gray"
            (draw (shifted 0 1.5 (ellipse-arc 0.5 1.5 0 pi/2))
                  (shifted 0 1.5 (ellipse-arc 0.5 1.5 (* 3/2 pi) 2pi)))))
          (shifted 0 1.5 (ellipse-arc 0.5 1.5 pi/2 (* 3/2 pi)))
          (curve (pt 0 0) -- (pt 4 1))
          (curve (pt 0 3) -- (pt 4 2))          
          (ellipse-curve 4 1.5 0.166 0.5)))))

