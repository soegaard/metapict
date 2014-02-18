#lang racket
;;;
;;; Pointilism - Animation
;;;

; Inspired by 
;   http://processing.org/examples/pointillism.html
; The image shows Scott on the moon saluting the american flag.

(require metapict 
         (only-in racket/gui image-snip%)
         (only-in 2htdp/universe big-bang on-tick to-draw)
         (only-in 2htdp/image overlay empty-scene))

(def bm (read-bitmap "moonlanding.jpg")) ; read bitmap from disk
(defv (w h) (bitmap-size bm))            ; determine width and height
(set-curve-pict-size w h)                ; tell MetaPict the physical size
(curve-pict-window (window 0 w h 0))     ; set logical coordinate system

(define (draw-points n size)                  ; draw n circles of radius size
  (for/draw ([n n])        
    (def x (random w))                        ; generate random point (w,h)
    (def y (random h))
    (def c (get-pixel bm x y))                ; find color of that point
    (color c (fill (circle (pt x y) size))))) ; draw disk of that color

(define (pict->scene p)
  (make-object image-snip% (pict->bitmap p)))

(define (handle-on-tick world)
  (defm (list size scene) world)
  (list size (draw scene (draw-points 100 size))))

(define (draw-world w)
  (pict->scene (second w)))

(big-bang (list 4 (blank w h))
          [on-tick handle-on-tick]
          [to-draw draw-world])
