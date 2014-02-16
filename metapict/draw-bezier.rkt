#lang racket/base
; TODO:
;  - & to append paths  (use curve-append)
;  - tight bounding boxes
;  - linear equations
;  - collect paths in pict-type?
;  - implement tensepath  (see MetaFontBook) it replaces -- with --- in resolved curves
; TODO : Fix placement of bottom of the text
; TODO : dc-path->curve  (used for text->curve)

; BUGS
;  - make (curve (pt 0 0) .. 'cycle) signal an error 
;  - fix: (path: (pt -1 0) down .. (pt 1 0) .. down (pt -1 1) .. (pt -1 0))
;  - fix: parsing of the path: (curve (pt u 0) up .. (pt (- u) 0) down -- cycle)
;  - fix: (curve A .. B -- C -- D .. cycle) breaks split-into-long-segments
;         due to open around the cycle:   .. cycle .. 
;  - fix (curve A .. B .. C right .. D .. cycle)
; Reference: http://tex.loria.fr/prod-graph/mp.pdf  See point 296.

; IDEAS
;  - spark lines
;  - functions from asymptote

(require "pict-lite.rkt" "metapict.rkt"
         racket/math)

(require 
 (except-in unstable/gui/pict 
            ; These names are overwritten:
            fill    ; should be named inset/centered
            color   ; just colorize with arguments swapped - our match expander handles this
            ))

(module+ test (require rackunit))



(def w 400)
(def h 400)
(def d (make-bitmap-dc w h))

(def T (stdtrans (window -1.5 1.5 -1.5 1.5) w h))  ; logical -> device
(def P1 ((xscaled (xpx 1 T)) (yscaled (ypx 20 T)))) ; pen 1px x 50px elliptical pen

(def P2 ((rotated (/ pi 6)) (yscaled (ypx 20 T)) (xscaled (xpx 1 T))))
; pen 1px x 50px elliptical pen
(def P3 ((yscaled (ypx 20 T)) (xscaled (xpx 1 T))))


;(send d set-pen (send the-pen-list find-or-create-pen "red" 1.2 'solid))
#;(draw-bezs d (path->bezs fullcircle) 
             #:transformation T
             #:pen-transformation P1)

; (send d set-pen (send the-pen-list find-or-create-pen "blue" 1 'solid))
#;(draw-bezs d (path->bezs fullcircle) 
             #:transformation T
             #:pen-transformation P3)

;(send d set-pen (send the-pen-list find-or-create-pen "black" 1 'solid))
#;(draw-bezs d (path->bezs (star 5 3)) 
             #:transformation T
             #:pen-transformation P2)

;(send d set-pen (send the-pen-list find-or-create-pen "black" 20 'solid))
;(draw-bezs d (path->bezs fullcircle) #:pen-transformation (yscaled 2))

;(send d get-bitmap)
;(paths->bitmap (list path-example-1 path-example-2))
;(display-path path-example-1)

;(inset ; extend the bounding box (the bezier curver binding box ignores pen width)
; (apply lt-superimpose
;        (for/list ([b (path->bezs path-example-1)])
;          (linewidth 0 (scale (bezier b) 100))))
; 5)
;
;(scale (colorize (linewidth 0 (filled-curve path-example-2)) "red") 100)
;
;(path->bitmap (triangle #:side 1))
;; (path->bitmap (polygon 5 1))
;
;(path->bitmap (spark-lines '(435 438 512 460 491 487 552 511 505 509))
;              #:window (window 0 12 400 560)
;              #:device-width (exact-floor (* 20 (/ (- 560 400) 12))) #:device-height 20
;              #:width 1)

#;(curve->pict (curve #f (resolve-path-to-bezs (open-path: (pt 0 0) up .. (pt 1 1 ) .. up (pt 2 0))))
               (window -2.1 2.1 -2.1 2.1) 100 100)

; (def dot-size (make-parameter 3))

#;(let () ; table of filled circle in different colors
    (set-curve-pict-size 20 20)
    (for/list ([i (in-range 0 1.1 .1)])
      (for/list ([j (in-range 0 1.1 .1)])
        (color (color-med i "red" (color-med j "green" "blue"))
               (fill unitcircle)))))

#;(let* ([a (color "red" (disk 20))]
         [b (color "blue" (filled-rectangle 20 20))]
         [p (vl-append a (hb-append (blank 100) b))])
    (set-curve-pict-size p)
    (curve-pict-window (window -1 1 1 -1))
    (def c (curve (devpt (rb-find p a)) .. (devpt (lt-find p b))))
    (ahlength (px 10))
    (draw p 
          (draw-arrow c)
          (label-urt (rotate (text "label") (rad -45)) (pt 0 0))))

"Example ''set line cap''  from http://www.cairographics.org/samples/"
(def (example-cairo-cap)
  (let ()
    (defv (w h) (values 300 300))
    (set-curve-pict-size w h)
    (curve-pict-window (window 0 w 0 h))
    (def three-lines
      (draw (pencap 'butt   (draw (curve (pt  64 50) -- (pt  64 200))))
            (pencap 'round  (draw (curve (pt 128 50) -- (pt 128 200))))
            (pencap 'square (draw (curve (pt 192 50) -- (pt 192 200))))))
    (draw (penwidth 30 three-lines)
          (color "red" (penwidth 2.56 three-lines)))))

"Example ''set line join''  from http://www.cairographics.org/samples/"
(define (example-cairo-join)
  (let ()
    (defv (w h) (values 300 300))
    (set-curve-pict-size w h)
    (curve-pict-window (window 0 w h 0))
    (def A (pt 76.8 84.48)) 
    (def B (pt+ A (vec 51.2 -51.2)))
    (def C (pt+ B (pt 51.2 51.2)))
    (def bend  (curve A -- B -- C))
    (penwidth 40.96 (draw (penjoin 'miter (draw bend))
                          (penjoin 'bevel (draw (shifted 0 (- 161.28 84.48) bend)))
                          (penjoin 'round (draw (shifted 0 (- 238.08 84.48) bend)))))))


#;(define (text-curve str font x y [combine? #f]) ; x y is the top left of the text
    (def dp (new dc-path%))
    (defv (size family style weight) (values 90 'roman 'normal 'bold))
    (def f (send the-font-list find-or-create-font size family style weight))
    (send dp text-outline f str x y combine?)
    (dc-path->curve dp)) ; waiting for dc-path->curve

"Example ''text''  from http://www.cairographics.org/samples/"
(def (example-cairo-text)
  (let () ; TODO : Fix placement of bottom of the text
    (defv (w h) (values 300 300))
    (set-curve-pict-size w h)
    (curve-pict-window (window 0 w h 0))
    (def face (cons 'bold "sans"))  
    ;(def void (shifted 70 165 (text-curve "void" face)))
    (def blueish (make-color* 128 128 1))
    (label-offset 0)
    (draw (label-urt (frame (text "Hello" face 90)) (pt 10 135))
          #;(fill (color blueish void))
          #;(penwidth 2.56 (color "black" void))
          (color (make-color* 255 (* .2 255) (* .2 255) .6)
                 (fill (shifted 10 135 (scaled 5.12 unitcircle))))
          #;(color (make-color* 255 (* .2 255) (* .2 255) .6)
                   (fill (shifted 70 165 (scaled 5.12 unitcircle)))))))

#;(module+ test
    (define (curve-tests)
      ; A .. B
      (for* ([i (in-range -1 2)]
             [j (in-range -1 2)]
             [k (in-range -1 2)]
             [l (in-range -1 2)]
             #:unless (pt= (pt i j) (pt k l)))
        (def A (pt i j))
        (def B (pt k l))
        (displayln (list A B))
        (define (check d)
          (andmap bez~ 
                  (curve:-bezs (apply make-curve d))
                  (curve:-bezs (path->curve/mpost (apply path: d)))))
        (check-true (check (list A            ..            B)))
        (check-true (check (list A (dir 15)   ..            B)))
        (check-true (check (list A            .. (dir 15)   B)))
        (check-true (check (list A (dir 15)   .. (dir 20)   B)))
        (check-true (check (list A (curl 1.5) ..            B)))
        (check-true (check (list A            .. (curl 1.5) B)))
        (check-true (check (list A (curl 1.5) .. (curl 2.5) B)))))
    (curve-tests))



(define (axes)
  (defm (window xmin xmax ymin ymax) (curve-pict-window))
  (draw (draw-arrow (curve (pt xmin 0) -- (pt xmax 0)))
        (draw-arrow (curve (pt 0 ymin) -- (pt 0 ymax)))))

(define (plot-xy xs ys)
  (draw (axes)
        (penscale 4 (color "red" (draw* (map pt xs ys))))))







