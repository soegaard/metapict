#lang racket/base
(require metapict metapict/mat)
(require racket/class)
(provide
 ; Configuration
 angle-radius
 angle-spacing
 mark-size
 mark-spacing
 ; marks (ticks)
 straight-mark
 slanted-mark
 arc-outline
 mark-angle
 mark-curve-angle
 mark-curve
 mark-interval
 marked-arc)
 
;;;
;;; CONFIGURATION
;;;

; It's a bit tricky to pick good defaults for the size and placement defaults
; for angle markings. For one using a thicker line requires more space between
; lines. This matters because there is a difference between bitmaps, svg and pdfs.
; For now, let's rely on parameters.

(define angle-radius  (make-parameter 0.7))   ; distance along left leg
(define angle-spacing (make-parameter 0.1)) ; distance between arcs
(define mark-size     (make-parameter 0.3)) ; size of straight mark
(define mark-spacing  (make-parameter 0.2)) ; space between side marks

;;; Automatic configuration

; Eventually the above configuration ought to be computed on the bases of the
; penwidth. The following functions aren't used now - but can be come in handy
; in the future.

; The actual penwidth isn't know until the pict is drawn, so we need some trickery:

(define (get-current-pen)
  ; This creates a fake pict, draw sets up the pen, and then we grab it.
  (let/ec return (draw (unsafe-dc (λ (dc dx dy) (return (send dc get-pen))) 1 1))))

(define (get-line-width [pen (get-current-pen)])  
  (send pen get-width))

(define (mm x) (* 2.83465 x)) ; mm to pt

; angle-radius : [pen] -> number
#;(define (angle-radius [pen (get-current-pen)])
    (+ (px (mm 8))
       (* angle-radius-factor
          (sqrt (get-line-width pen)))))

#;(define (angle-space [pen (get-current-pen)])
    (* angle-space-factor
       (sqrt (get-line-width pen))))

;;;
;;; Individual marks
;;;

; There are different styles to putting marks on the arc indicating the angle.
; Here we define a few simple ones.

; normalize : vec -> vec
;  return unit vector in the same direction
(define (normalize v)
  (vec* (/ 1. (norm v)) v))

; straight-mark : curve number -> pict
;   draw straight mark on the curve c in a distance a from the start point.
(define (straight-mark c a [at #f] #:size [size (mark-size)])
  (def t (or at (arc-time c a)))
  (def p (point-of c t))
  (def v (vec* (* 0.5 size) (rot90 (normalize (direction-of c t)))))
  (def d (draw (shifted p (curve (pt+ p v) .. (pt- p v)))))
  (draw (curve (pt+ p v) .. (pt- p v))))

(define (slanted-mark c a [at #f] #:size [size (mark-size)])  
  (def t (or at (arc-time c a)))
  (def p (point-of c t))
  (def v (rot -15 (vec* (* 0.5 size) (rot90 (normalize (direction-of c t))))))
  (def d (draw (shifted p (curve (pt+ p v) .. (pt- p v)))))
  (draw (curve (pt+ p v) .. (pt- p v))))

; mark-interval : curve (curve number -> pict) number bool -> pict
;   Place n marks along the curve c.
;   Note: Use subcurve if you need to place marks along a piece of a curve.
(define (mark-interval c [n 1] #:marker [marker straight-mark])
  (def l (arc-length c))
  (def dl (/ l (+ n 1)))
  (for/draw ([i n])
    (def a (* (+ i 1) dl))
    (def t (arc-time c a))
    (def p (point-of c t))
    (marker c a t)))

(define (marked-arc p q r #:marks [n 0] #:marker [marker straight-mark])
  ; Draw the arc from q to r with center p using n marks.
  (def c (arc p q r))
  (if (= n 0)
      (draw c)
      (draw c (mark-interval c n #:marker marker))))

(define (make-arc-marker n [marker straight-mark])
  (λ (p q r #:marks [n n] #:marker [marker marker])
    (marked-arc p q r #:marks n #:marker marker)))

(define (arc-outline p q r) ; p is center
  ; return curve of outline (it is pizza shaped)
  (curve-append (curve r -- p -- q) (arc p q r)))

; mark-curve-angle : curve curve number number number [#:draw-angle-curve bool] -> pict
;   The two curves are expected to have the same start point P.
;   This creates and "angle" between curve c1 and curve c2.
;   The number    radius   is the distance along c1 from P to the beginning of the first arc.
;   The number    spacing  is the distance between arcs.
;   The number    n        is the number of marks (ticks) on drawn on each arc.
;   The function  marker   is a function that can draw marks (ticks).
;   The function draw-angle-curve can be used to draw other curves than plain arcs.
(define (mark-curve-angle c1 c2
                          #:arcs             [n 1]    ; number of arcs
                          #:marks            [m 0]    ; number of marks on each arc
                          #:radius           [radius  (angle-radius)]
                          #:spacing          [spacing (angle-spacing)]
                          #:marker           [marker  straight-mark]
                          #:draw-angle-curve [draw-angle-curve marked-arc])
  ; The two curves c1 and c2 are expected to begin in the same point P.
  (def p (start-point c1))
  ; Draw n arcs from c1(t) to c2(t).
  (for/draw ([i n])
    ; The distance from P along c1 to the start point of the first arc is r.  
    ; Find start point of arc on the first curve c1
    (def t (arc-time c1 (+ radius (* i spacing))))
    (def q (point-of c1 t))
    ; Find end point of arc with center p through q over to curve c2
    (def r (intersection-point c2 (circle p (dist p q))))
    ; draw
    (draw-angle-curve p q r #:marks m #:marker marker)))

; mark-angle : point point point [keyword-arguments ...] -> pict
;   The points A, B and C make and angle at B.
;   The angle arcs are drawn in the positive orientation.
;   The number    radius   is the distance along c1 from P to the beginning of the first arc.
;   The number    spacing  is the distance between arcs.
;   The number    n        is the number of marks (ticks) on drawn on each arc.
;   The function  marker   is a function that can draw marks (ticks).
;   The function  draw-angle-curve can be used to draw other curves than plain arcs.
;   The boolean   fill?    indicates that the angle should be filled (with the current brush).
(define (mark-angle a b c
                    #:arcs             [n 1]                     ; number of arcs
                    #:marks            [m 0]                     ; number of marks
                    #:marker           [marker  straight-mark]
                    #:spacing          [spacing (angle-spacing)]
                    #:radius           [radius  (angle-radius)]
                    #:draw-angle-curve [draw-angle-curve marked-arc]
                    #:fill?            [fill? #f])
  ; Center of arc
  (def p b) 
  ; Draw n arcs
  (draw
   ; fill ?
   (if fill?
       (let ()
         (def q (pt+ b (vec* radius (normalize (pt- a b)))))
         (def r (pt+ b (vec* radius (normalize (pt- c b)))))
         (fill (arc-outline p q r)))
       (blank))
   ; draw n arcs
   (for/draw ([i n])
     ; angle radius
     (def min-radius (- radius (* (/ (- n 1) 2) spacing)))
     (def rad (+ min-radius (* i spacing)))
     ; start and end point
     (def q (pt+ b (vec* rad (normalize (pt- a b)))))
     (def r (pt+ b (vec* rad (normalize (pt- c b)))))
     ; draw
     (draw-angle-curve p q r #:marks m #:marker marker))))

; mark-right-angle : point point point [keyword-arguments ...] -> pict
;     The points A, B, and C make a right angle at B (or an angle that is presumed to be right).
;     The angle ABC is marked with two line segments that form a square/rhombus against the
;     legs of the angle.
(define (mark-right-angle a b c
                          #:radius [radius (angle-radius)]
                          #:fill? [fill? #f])
  (def p b)
  (def q (pt+ b (vec* radius (normalize (pt- a b)))))
  (def r (pt+ b (vec* radius (normalize (pt- c b)))))
  (def s (pt+ q (pt- r b)))
  (draw
   (if fill?
       (fill (curve p -- q -- s -- r -- cycle))
       (blank))
   (penjoin 'miter (draw (curve q -- s -- r)))))

(define (mark-curve c n [α .5]
                    #:marker  [marker  straight-mark]
                    #:spacing [spacing (mark-spacing)])
  (def l (arc-length c))
  (def mid (* α l))
  (for/draw ([i n])
    (def spaces (- n 1))    
    (def a (+ (- mid (* (/ spaces 2) spacing))
              (* i spacing)))
    (def t (arc-time c a))
    (marker c a t)))
  

#;(begin
(define circ (penscale 8 (draw (pt 0 0))))

(set-curve-pict-size 200 200)

;;; Examples of marks

(with-window (window/aspect -1.5 1.5)
  (def c1 (curve (pt -1  1) -- (pt 1  1)))
  (def c2 (curve (pt -1  0) -- (pt  1 0)))
  (def c3 (curve (pt -1 -1) -- (pt 1 -1)))
  (draw c1 (mark-interval c1 3)
        c2 (mark-interval c2 3 #:marker slanted-mark)))

; Example: Different number of angle arcs
(with-window (window -1.1 6.1 -1.1 6.1)
  ; Triangle SSA  with b=5, c=4, ∠A=30º
  (def A origo)
  (def C (pt+ A (vec 5 0)))  ; AC = b = 5
  (def B (pt@d 4 30))        ; |AB|=c=3,  ∠A=30º
  (def AB (curve A .. B))
  (def AC (curve A .. C))
  (def BC (curve B .. C))
  (draw AB AC BC
        ; use arcs to mark the angles
        (color "red"   (mark-angle C A B #:arcs 1))
        (color "blue"  (mark-angle B C A #:arcs 2))
        (color "green" (mark-angle A B C #:arcs 3))
        ; put ticks on the sides
        (mark-curve BC 1)
        (mark-curve AB 2)
        (mark-curve AC 3)
        ; labels
        (label-lft "A" A)
        (label-top "B" B)
        (label-rt  "C" C)))

; Example: Angles arc filled with color
(with-window (window -1.1 6.1 -1.1 6.1)
  ; Triangle SSA  with b=5, c=4, ∠A=30º
  (def A origo)
  (def C (pt+ A (vec 5 0)))  ; AC = b = 5
  (def B (pt@d 4 30))        ; |AB|=c=3,  ∠A=30º
  (def AB (curve A .. B))
  (def AC (curve A .. C))
  (def BC (curve B .. C))
  (draw AB AC BC
        (color "red"   (mark-angle C A B #:fill? #t))
        (color "blue"  (mark-angle B C A #:fill? #t))
        (color "green" (mark-angle A B C #:fill? #t))
        (mark-angle C A B #:marks 1)
        (mark-angle B C A #:marks 2)
        (mark-angle A B C #:marks 3)
        (label-lft "A" A)
        (label-top "B" B)
        (label-rt  "C" C)))
  


(with-window (window -5 5 -5 5)
  (def c (circle 4))
  (draw c
        (curve (pt -5 -5) -- (pt -5 5) -- (pt 5 5) -- (pt 5 -5) -- (pt -5 -5))
        (color "red" (mark-interval c 15))))

(with-window (window -5 5 -5 5)
  (def c1 (curve (pt 0 0) .. (pt 2 1) .. (pt 5 0)))
  (def c2 (curve (pt 0 0) .. (pt 5 5)))
  (draw c1 c2
        (mark-curve-angle c1 c2 #:arcs 3 #:radius (px 20) #:spacing (px 5))))

(with-window (window -5 5 -5 5)
  (def c (curve (pt -2 0) .. (pt 2 3) .. (pt 4 1)))
  (draw c
        (mark-interval c 6)))

; Example: marked right angles
(def A (pt (* -1/2 (sqrt 2)) (* -1/2 (sqrt 2))))
(def B (pt (* 1/2 (sqrt 2)) (* -1/2 (sqrt 2))))
(def C (pt 0 1))
(def D (med 0.5 A B))
(def E (pt 0 -1))
(def F (pt 0 0))
(def G (pt 0.4 0.1))

(with-window (window -1.3 1.3 -1.3 1.3)
  (draw
   unitcircle
   (mark-right-angle C D A #:radius 0.1)
   (brushcolor (color-med 0.7 "red" "white")
                               (mark-right-angle G D B #:radius 0.1 #:fill? #t))
   (dashed (color "red" (draw (curve G -- A))))
   (dashed (color "red" (draw (curve G -- B))))
   (dashed (color "red" (draw (curve G -- D))))
   (curve A -- B)
   (curve C -- E)
   (label-llft "A" A)
   (label-lrt "B" B)
   (label-top "C" C)
   (label-llft "D" D)
   (label-bot "E" E)
   (dot-label-lft "F" F)
   (color "red" (dot-label-top "G" G))))
)