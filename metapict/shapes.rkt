#lang racket
;;; Curves with common shapes.

(provide unitcircle      ; center (0,0) radius 1
         fullcircle      ; center (0,0) diameter 1  (radius 1/2)
         halfcircle      ; upper part of fullcircle
         quartercircle   ; right part of halfcircle
         unitsquare      ; side length 1, first quadrant
         arc             ; arc of circle of radius r from f to t (f and t is in radian)
         arc/deg         ; same, f and t in degrees
         circle-curve    ; circle with center (x0,y0) and radius r
         ellipse-curve   ; ellipse with center (x0,y0) and width w and height h
         sector          ; sector of radius r with angles from f to t
         sector/deg      ; same but angles are in degrees rather than radians
         )     

(require "def.rkt" "curve.rkt" "pt-vec.rkt" "path.rkt" "trig.rkt" "trans.rkt" "structs.rkt")

(def unitcircle
  (curve (pt@ 1 0)         (dir  90) ..
         (pt@ 1 (rad  45)) (dir 135) ..
         (pt@ 1 (rad  90)) (dir 180) ..
         (pt@ 1 (rad 135)) (dir 225) ..
         (pt@ 1 (rad 180)) (dir 270) ..
         (pt@ 1 (rad 225)) (dir 315) ..
         (pt@ 1 (rad 270)) (dir   0) ..
         (pt@ 1 (rad 315)) (dir  45) .. (dir 90)          
         (pt@ 1 0)))

(def quartercircle (curve (pt@ 1 0) (dir  90) .. (dir 180) (pt@ 1 (rad  90))))
(def halfcircle    (curve-append quartercircle ((rotatedd 90) quartercircle)))
(def fullcircle
  (curve (pt@ 1/2 0)         (dir  90) ..
         (pt@ 1/2 (rad  45)) (dir 135) ..
         (pt@ 1/2 (rad  90)) (dir 180) ..
         (pt@ 1/2 (rad 135)) (dir 225) ..
         (pt@ 1/2 (rad 180)) (dir 270) ..
         (pt@ 1/2 (rad 225)) (dir 315) ..
         (pt@ 1/2 (rad 270)) (dir   0) ..
         (pt@ 1/2 (rad 315)) (dir  45) .. (dir 90) cycle))

(def unitsquare (curve (pt 0 0) -- (pt 1 0) -- (pt 1 1) -- (pt 0 1) -- cycle))

(define (circle-curve . args)
  (match args
    [(list center-x center-y radius)
     (shifted center-x center-y (scaled radius unitcircle))]
    [(list center radius)
     (shifted (pt-x center) (pt-y center) (scaled radius unitcircle))]
    [_ (error 'circle-curve (~a "got: " args))]))
    

#;(define (circle-curve center-x center-y radius)
  (shifted center-x center-y 
           (scaled radius unitcircle)))

(define (ellipse-curve center-x center-y radius-x radius-y)
  (shifted center-x center-y 
           (yscaled radius-y (xscaled radius-x unitcircle))))

;; Circle related

(define (arc radius from-angle to-angle) ; radians
  ; TODO: handle angles outside normal range
  ; TODO: add optional center for arc
  (subcurve (scaled radius unitcircle)
            (* 8 (/ from-angle 2pi))
            (* 8 (/ to-angle 2pi))))

(define (arc/deg radius from to)
  (arc radius (rad from) (rad to)))

(define (sector radius from to)
  (def a  (arc radius from to))
  (def a0 (point-of a 0))
  (def a1 (point-of a (curve-length a)))
  (curve-append (curve (pt 0 0) -- a0)
                a
                (curve a1 -- (pt 0 0))))

(define (sector/deg r from to)
  (sector r (rad from) (rad to)))
  
  
  
  
  
  
