#lang racket
(require metapict)
;;;
;;; Pointilism
;;;

; Inspired by 
;   http://processing.org/examples/pointillism.html

; The image shows Scott on the moon saluting the american flag.

(def bm (read-bitmap "moonlanding.jpg")) ; read bitmap from disk
(defv (w h) (bitmap-size bm))            ; determine width and height

(define (draw-points n size)             ; draw n circles of radius size
  (for/draw ([n n])
    ; generate random point (w,h)
    (def x (random w))
    (def y (random h))
    ; find color of the (x,y)th pixel in the image
    (def c (get-pixel bm x y))
    ; use the color to draw a filled circle
    (color c (fill (circle (pt x y) size)))))

(set-curve-pict-size 300 300)
(with-window (window 0 w h 0)
  (draw (draw-points  100 40)
        (draw-points  200 20)
        (draw-points  400 10)
        (draw-points 4000  4)))

