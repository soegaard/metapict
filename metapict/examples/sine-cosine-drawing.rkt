#lang racket
(require metapict)
; todo : fix arc to handle angles outside the normal range

;;; 
;;; EXAMPLE  
;;;   This examples is inspired by:
;;;       Example: A picture for Karl’s students
;;;       http://www.texample.net/tikz/examples/tutorial/
;;;   Compare with:
;;;       https://github.com/takikawa/pict-utils

(def x-axis (curve (pt -1.5 0) .. (pt 1.5 0)))
(def y-axis (curve (pt 0 -1.5) .. (pt 0 1.5)))
(def xticks '(-1 -0.5 1))
(def yticks '(-1 -0.5 0.5 1))

(set-curve-pict-size 400 400)
(with-window (window -1.5 1.5 -1.5 1.5)
  (current-label-text-size 10)
  (penscale 2
  (draw (color "gray" (grid (pt -1.4 -1.4) (pt 1.4 1.4) (pt 0 0) #:step 0.5))
        (draw-arrow x-axis)
        (draw-arrow y-axis)
        (circle 0 0 1)
        (draw* (for/list ([x xticks]) (curve (pt x -0.05) -- (pt x 0.05))))
        (draw* (for/list ([y yticks]) (curve (pt -0.05 y) -- (pt 0.05 y))))
        (draw* (for/list ([x xticks]) (fill-label "white" (label-bot (~a x) (pt x 0)))))
        (draw* (for/list ([y yticks]) (fill-label "white" (label-lft (~a y) (pt 0 y)))))
        (filldraw (sector/deg 0.3 0 30)
                  (color-med .80 "green" "white") (color-med .20 "green" "white"))
        (color "red"    (draw (curve (pt@ 1 (rad 30)) -- (pt (cos (rad 30)) 0))))
        (color "blue"   (draw (curve (pt 0 0)         -- (pt (cos (rad 30)) 0))))
        (color "orange" (draw (curve (pt 1 0)         -- (pt 1 (tan (rad 30))))))
        (draw (curve (pt 0 0) -- (pt 1 (tan (rad 30))))))))
