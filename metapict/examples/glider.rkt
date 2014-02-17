#lang racket
(require metapict)

;;; EXAMPLE
;;;   Glider, Hacker Emblem
;;;   http://www.texample.net/tikz/examples/glider/

(def circ (circle 0.5 0.5 0.42))

(with-window (window 0 3 0 3)
  (margin 5
    (for/fold ([drawing (draw (grid (pt 0 0) (pt 3 3) (pt 0 0) 1))])
      ([p (list (pt 0 0) (pt 1 0) (pt 2 0) (pt 2 1) (pt 1 2))])
      (draw drawing (fill (shifted p circ))))))

