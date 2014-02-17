#lang racket
;;; A Reuleaux triangle is a curve of constant width
(require metapict)
(set-curve-pict-size 300 300)

(defv (A B C) (values (pt@d 1 90) (pt@d 1 210) (pt@d 1 330)))
(draw (curve A -- B -- C -- cycle)
      (draw (arc A B C))
      (draw (arc B C A))
      (draw (arc C A B)))



