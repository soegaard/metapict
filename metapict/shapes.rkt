#lang racket/base
;;; Curves with common shapes.

(provide unitcircle        ; center (0,0) radius 1
         fullcircle        ; center (0,0) diameter 1  (radius 1/2)
         halfcircle        ; upper part of fullcircle
         quartercircle     ; right part of halfcircle
         unitsquare        ; side length 1, first quadrant
         arc               ; arc of circle of radius r from f to t (f and t is in radian)
                           ; or: (arc c a b) arc with center c from a to b
         arc/deg           ; same, f and t in degrees
         arc-from          ; (arc-from p start stop radius) arc starts at p angles are in degrees
         circle            ; circle with center (x0,y0) and radius r
         ellipse-curve     ; ellipse with center (x0,y0) and width w and height h
         ellipse-arc
         sector            ; sector of radius r with angles from f to t
         sector/deg        ; same but angles are in degrees rather than radians
         rectangle         ; rectangle given two opposite points, or point and diagonal vector
         rounded-rectangle ; same except for rounded corners
         )

(require "angles.rkt" "def.rkt" "curve.rkt" "pt-vec.rkt" "path.rkt" 
         "trig.rkt" "trans.rkt" "structs.rkt"
         racket/match racket/format)

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

(define (circle . args)
  (match args
    [(list (? real? r))                          (scaled r unitcircle)]
    [(list (pt x y) (? real? r))                 (shifted x y (scaled r unitcircle))]
    [(list (? real? x) (? real? y) (? real? r))  (shifted x y (scaled r unitcircle))]
    [(list (and (pt x y) C) (? pt? P))           (def r (dist C P))
                                                 (shifted x y (scaled r unitcircle))]
    [_ (error 'circle (~a "got: " args))]))


(define (ellipse-curve center-x center-y radius-x radius-y)
  (shifted center-x center-y 
           (yscaled radius-y (xscaled radius-x unitcircle))))

;; Circle related

#;(define (arc radius from-angle to-angle) ; radians
  ; TODO: handle angles outside normal range
  ; TODO: add optional center for arc
  (subcurve (scaled radius unitcircle)
            (* 8 (/ from-angle 2pi))
            (* 8 (/ to-angle 2pi))))

(define (arc . args)
  (match args
    [(list (? real? r) (? real? from) (? real? to)) ; angles in radian
     (cond [(>= from 2pi) (arc r (- from 2pi) (- to 2pi))]
           [(<  from 0)   (arc r (+ from 2pi) (+ to 2pi))]
           [(> from to)   (curve-append (arc r from 2pi) (arc r 0 to))]
           [else          (subcurve (scaled r unitcircle)
                                    (* 8 (/ from 2pi))
                                    (* 8 (/ to   2pi)))])]
    [(list (? pt? C) (? pt? A) (? pt? B)) ; arc through A with center C
     (def from-angle (angle (pt- A C)))
     (def to-angle   (+ from-angle (abs (angle2 (pt- C A) (pt- C B)))))
     (shifted C (arc (dist C A) from-angle to-angle))]
    [_ (error 'arc "")]))

(define (arc/deg radius from to)
  (arc radius (rad from) (rad to)))

(define (arc-from p radius start stop)
  ; arc starts at p, the angle (in degrees) start
  ; with  start degrees and ends with stop degrees
  ; (measured from the x-axis), the radius of the arc is radus.
  (def center (pt- p (vec* radius (dir start))))
  (shifted (shifted (vec* (- radius) (dir start)) p)
           (arc/deg radius start stop)))


(define (ellipse-arc x-radius y-radius from-angle to-angle) ; radians
  ; TODO: handle angles outside normal range
  ; TODO: add optional center for arc
  (subcurve (yscaled y-radius (xscaled x-radius unitcircle))
            (* 8 (/ from-angle 2pi))
            (* 8 (/ to-angle 2pi))))

(define (sector radius from to)
  (def a  (arc radius from to))
  (def a0 (point-of a 0))
  (def a1 (point-of a (curve-length a)))
  (curve-append (curve (pt 0 0) -- a0)
                a
                (curve a1 -- (pt 0 0))))

(define (sector/deg r from to)
  (sector r (rad from) (rad to)))
  
(define (rectangle a1 a2)
  (match (list a1 a2)
    [(list (pt x y) (vec dx dy)) ; point and diagonal vector
     (rectangle a1 (pt (+ x dx) (+ y dy)))]
    [(list (pt x y) (pt X Y))    ; opposite points
     (defv (xmin xmax ymin ymax) (values (min x X) (max x X) (min y Y) (max y Y)))
     (curve (pt xmin ymin) -- (pt xmax ymin) -- (pt xmax ymax) -- (pt xmin ymax) -- cycle)]
    [_ (error 'rectangle (~a "expected two points, or a point and a vec, got: " a1 " and " a2))]))


; rounded-rectangle : pt pt-or-vec [number] -> curve
;   Given two points of diagonally opposite corners in a rectangle,
;   returns a curve with the shape of a rectangle with rounded corners.
;   Alternatively the input can be one point and a vector to the
;   the diagonally opposite corner.
(define (rounded-rectangle a1 a2 [radius (* 8 803/800)])  ; mp default is 8bp = 803/800 pt
  (match (list a1 a2)
    [(list (pt x y) (vec dx dy)) ; point and diagonal vector
     (rounded-rectangle a1 (pt (+ x dx) (+ y dy)))]
    [(list (pt x y) (pt X Y))    ; opposite points
     (defv (xmin xmax ymin ymax) (values (min x X) (max x X) (min y Y) (max y Y)))
     (def r (min radius (* 0.5 (- xmax xmin)) (* 0.5 (- ymax ymin))))
     (curve          (pt (+ xmin r)    ymin)
            --       (pt (- xmax r)    ymin)    right 
            .. up    (pt    xmax    (+ ymin r))
            --       (pt    xmax    (- ymax r)) up
            .. left  (pt (- xmax r)    ymax)
            --       (pt (+ xmin r)    ymax)    left
            .. down  (pt    xmin    (- ymax r))
            --       (pt    xmin    (+ ymin r)) down
            .. right (pt (+ xmin r)    ymin)
            .. cycle)]
    [_ (error 'rounded-rectangle (~a "expected two points, or a point and a vec, got: " a1 " and " a2))]))
