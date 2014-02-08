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


#;(define (grid lower-left upper-right reference step)
  ; Draw a grid with the rectangle given by the lower-left 
  ; and upper-right corner. Reference is a point in
  ; which the horizontal and vertical lines intersection.
  ; step is the distance between lines.
  (defm (pt x0 y0) lower-left)
  (defm (pt x1 y1) upper-right)
  (defm (pt rx ry) reference)
  (draw* (append (for/list ([x (in-range rx x1 step)])
                   (curve (pt x y0) -- (pt x y1)))
                 (for/list ([x (in-range rx x0 (- step))])
                   (curve (pt x y0) -- (pt x y1)))
                 (for/list ([y (in-range ry y1 step)])
                   (curve (pt x0 y) -- (pt x1 y)))
                 (for/list ([y (in-range ry y0 (- step))])
                   (curve (pt x0 y) -- (pt x1 y))))))

(def x-axis (curve (pt -1.5 0) .. (pt 1.5 0)))
(def y-axis (curve (pt 0 -1.5) .. (pt 0 1.5)))
(def xticks '(-1 -0.5 1))
(def yticks '(-1 -0.5 0.5 1))

(set-curve-pict-size 400 400)
(with-window (window -1.5 1.5 -1.5 1.5)
  (current-font-size 10)
  (penscale 2
  (draw (color "gray" (grid (pt -1.4 -1.4) (pt 1.4 1.4) (pt 0 0) 0.5))
        (draw-arrow x-axis)
        (draw-arrow y-axis)
        (circle-curve 0 0 1)
        (draw* (for/list ([x xticks]) (curve (pt x -0.05) -- (pt x 0.05))))
        (draw* (for/list ([y yticks]) (curve (pt -0.05 y) -- (pt 0.05 y))))
        (draw* (for/list ([x xticks]) (fill-label "white" (label-bot (~a x) (pt x 0)))))
        (draw* (for/list ([y yticks]) (fill-label "white" (label-lft (~a y) (pt 0 y)))))
        (filldraw (sector/deg 0.3 0 30)
                  (color-med .20 "green" "white") (color-med .80 "green" "white"))
        (color "red"    (draw (curve (pt@ 1 (rad 30)) -- (pt (cos (rad 30)) 0))))
        (color "blue"   (draw (curve (pt 0 0)         -- (pt (cos (rad 30)) 0))))
        (color "orange" (draw (curve (pt 1 0)         -- (pt 1 (tan (rad 30))))))
        (draw (curve (pt 0 0) -- (pt 1 (tan (rad 30))))))))
