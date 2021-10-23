#lang racket/base

;;;
;;; Coordinate System and Points
;;;

; This module implements a coordinate system that is to be used as part of a drawing.
; Use a coordinate system, if you want to draw a mathematical coordinate system.
; In other words this module is not needed for other types of metapict drawings.

; A coordinate system consistes of two (mathematical) coordinate axes.
; A point in the coordinate system is represented as a point
; struct with a coordinate system and a some coordinates (a pt).
; I.e. the pt represents the coordinates, not a point.

; (struct system: (axis1 axis2)  #:transparent)
; (struct point: (system p)      #:transparent)

(provide
 ; selectors
 first-axis
 second-axis
 ; constructors
 system ; given origin and two basis vectors construct a coordinate system
 point 
 ; conversion
 point->pt
 ; fomatters
 format-point
 ; drawers
 system-grid 
 unit-labels
 system-ticks
 draw-point
 ; readings
 show-value-reading-x
 show-value-reading-y
 )

(require racket/format racket/list
         "axis.rkt" "curve.rkt"
         "def.rkt" "device.rkt" "draw.rkt" "mat.rkt"
         "metapict.rkt"
         "parameters.rkt" "pt-vec.rkt" "structs.rkt"
         "shapes.rkt")


;; CONSTRUCTORS

(define (system origin basis1 basis2 [label1 #f] [label2 #f])
  ; origin and basis basis vectors are in logical coordinates
  (def axis1 (axis origin basis1 label1))
  (def axis2 (axis origin basis2 label2))
  (system: origin axis1 axis2))

(define point
  (case-lambda
    [(system x y) (point: system (pt x y))]
    [(system pt)  (point: system pt)]))

;; SELECTORS

(define (first-axis system)
  (defm (system: _ a1 _) system)
  a1)

(define (second-axis system)
  (defm (system: _ _ a2) system)
  a2)


;;; CONVERTERS

(define (point->pt point)
  (defm (point: s p) point)
  (defm (pt x y) p)
  (defm (system: o a1 a2) s)
  (defm (axis _ i _) a1)
  (defm (axis _ j _) a2)
  (pt+ (pt+ o (vec* x i))
       (vec* y j)))

(current-point-to-pt-converter point->pt)

(define (pt->point s p)
  (defm (pt x y) p)
  (defm (system: o (axis _ i _) (axis _ j _)) s)
  (defm (vec i1 i2) i)
  (defm (vec j1 j2) j)
  ; X i + Y j = p
  (def m (mat i1 j1
              i2 j2))
  (defm (vec X Y) (mat*vec (mat-inv m) (pt- p origo)))
  (point s X Y))

;;; FORMATTERS

(define (format-point point)
  (defm (point: s (pt x y)) point)
  (~a "(" x "," y ")"))

;;; DRAWERS

(define (draw-system system)
  (defm (system: o a1 a2) system)
  (draw a1 a2))

(current-draw-system draw-system)

(define (draw-point point)
  (fill (circle (point->pt point) (px 2))))

(current-draw-point draw-point)

(define (unit-labels s)
  (defm (system: o a1 a2) s)
  (draw (unit-label a1)
        (unit-label a2)))

(define (system-ticks s #:size [size 0.1] #:size1 [size1 #f] #:size2 [size2 #f])
  (defm (system: o a1 a2) s)
  (def s1 (or size1 size))
  (def s2 (or size2 size))
  (draw (ticks a1 #:size s1)
        (ticks a2 #:size s2)))


;;; EXAMPLE

(define (show-value-reading-x p
                              #:show-value [show? #t]
                              #:label      [label #f]
                              #:color      [col   "black"]
                              #:penscale   [ps 1])
  (defm (point: s (pt x y)) p)
  (def px (point s (pt x 0)))  
  (draw (penscale ps (color col (dashed (draw (curve (point->pt p) -- (point->pt px))))))
        (and (or label show?)
             (label-bot (or label (~a x)) (point->pt px)))))

(define (show-value-reading-y p
                              #:show-value [show? #t]
                              #:label      [label #f]
                              #:color      [col   "black"]
                              #:penscale   [ps 1])
  (defm (point: s (pt x y)) p)
  (def py (point s (pt 0 y)))  
  (draw (penscale ps
          (color col (dashed (draw (curve (point->pt p) -- (point->pt py))))))
        (and (or show? label)
             (label-lft (or label (~a y)) (point->pt py)))))


(define (system-grid s #:last-tick? [ts? #t] #:first-tick? [ft? #f])
  (defm (system: origin a1 a2) s)
  (def xs (tick-ordinates a1 #:last-tick? ts? #:first-tick? ft?))
  (def ys (tick-ordinates a2 #:last-tick? ts? #:first-tick? ft?))
  (def bl (point s (first xs) (first ys)))
  (def ur (point s (last  xs) (last  ys)))
  (def xmin (first xs))
  (def xmax (last xs))
  (def ymin (first ys))
  (def ymax (last ys))
  (list
   (for/list ([x xs])
     (curve (point->pt (point s x ymin)) -- (point->pt (point s x ymax))))
   (for/list ([y ys])
     (curve (point->pt (point s xmin y)) -- (point->pt (point s xmax y))))))

;; (set-curve-pict-size 400 400)
;; (ahlength (px 6))
;; (label-offset 12) ; in output units

;; (def s  (system (pt 0 0) (vec .2 0) (vec 0 .2)))
;; (def a1 (first-axis s))
;; (def a2 (second-axis s))
;; (def p  (point s 1 1))p
;; (def q  (point s 3 2))


;; (draw (color "gray" (draw (system-grid s)))
;;       (show-value-reading-x p )
;;       (show-value-reading-x q #:color "red" #:penscale 2)
;;       (show-value-reading-y q #:color "red" #:penscale 2)
;;       s p q
;;       (label-rt (format-point p) (point->pt p))
;;       (label-rt (format-point q) (point->pt q))
;;       (ticks a1 #:size .2)
;;       (ticks a2 #:size .2)
;;       (unit-label a1)
;;       (unit-label a2)
;;       #;(tick-labels a1))
