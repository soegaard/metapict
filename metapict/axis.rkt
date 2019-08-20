#lang racket/base
; TODO: different types of ticks (one sided, slantes etc)
; TODO: tick labels need to use tick size to give a better placement

(require racket/list racket/match racket/math racket/format
         "metapict.rkt" "parameters.rkt")

; An axis consist of an origin (point where 0 is placed) and a unit-vector,
; which is a vector from the origo to the point where 1 is placed.
; The coordinates of origin and unit-vector are logical coordinates.

; An axis consist of an origin (point where 0 is placed) and a unit-vector,
; which is a vector from the origo to the point where 1 is placed.
; The coordinates of origin and unit-vector are logical coordinates.

; (struct axis   (origin unit-vector) #:transparent)
; (struct system (axis1 axis2)        #:transparent)
; (struct point  (system pt)          #:transparent)



(provide (all-defined-out))

(provide axis          ; make new axis given position of origo and direction
         axis-dir      ; direction vector (logical coordinates)
         axis-origin   ; position of 0    (logical coordinates)
         visible-range ; two values: start and end in axis ordinates
         draw-axis
         coordinate->pt ; axis and xa to logical pt
         tick-center    ; given axis and x to position of tick center
         tick           ; given axis and x to curve of a tick
         ticks          ; produce list of tick curves in visible range
         tick-label     ; axis x to label next to tick
         tick-labels    ; 
         unit-label)
         

(define (axis-dir a)
  (defm (axis o v) a)
  v)

(define (coordinate a p)
  ; given a point p along the axis a, return the coordinate of p
  ; p is given in logical coordinates
  
  (defm (axis o v) a)
  ; axis unit size (in logical units)
  (def u (norm v))
  ; number of units 
  (def c (/ (dist o p) u))
  (if (< (angle2 v (pt- p o)) π/2)
      c
      (- c)))

(define (visible-range a win)
  ; return the start and end (in axis coordinate)
  ; of range in which the axis is visible
  (defm (axis o v) a)
  (defm (window minx maxx miny maxy) win)
    
  ; clipping box
  (def left-border  (curve (pt minx miny) -- (pt minx maxy)))
  (def right-border (curve (pt maxx miny) -- (pt maxx maxy)))
  (def lower-border (curve (pt minx miny) -- (pt maxx miny)))
  (def upper-border (curve (pt minx maxy) -- (pt maxx maxy)))
  ; find intersection points of axis and clipping box
  (define (line p v) (curve (pt+ p (vec* 10000 v)) -- (pt+ p (vec* -10000 v))))
  (def l (line o v))
  (def p (first (append (intersection-points l left-border)
                        (intersection-points l lower-border)
                        (list #f))))
  (def q  (first (append (intersection-points l right-border)
                         (intersection-points l upper-border)
                         (list #f))))
  (match* (p q)
    [(#f #f)  (values #f #f)]  ; not visible
    [(p q)    (values (coordinate a p)
                      (coordinate a q))]))
  

; The axis is drawn as a line ending in an arrow.
; The axis is conceptually infinite, so we need a logical window
; so we can find the placement of the arrow.

(define (draw-axis a #:window [win (curve-pict-window)])
  (defv (xmin xmax) (visible-range a win))
  (define (coord x) (coordinate->pt a x))
  (parameterize ([ahlength (px 8)])
    (draw-arrow (curve (coord xmin) -- (coord xmax)))))

(current-draw-axis draw-axis)

(define (coordinate->pt a x)
  ; given an axis a and a coordinate x in axis units,
  ; return the corresponding point in logical units
  (defm (axis o v) a)
  (pt+ o (vec* x v)))

(define (tick-center a x)
  (coordinate->pt a x))

(define (tick axis x #:size [s (get current-tick-size)])
  (def p (tick-center axis x))
  (def v (vec* s (rot90 (axis-dir axis))))
  (curve (pt+ p v) -- (pt- p v)))


(define (ticks a      ; axis
               [d 1]  ; axis units between ticks
               #:size   [ts   (get current-tick-size)]
               #:window [win (curve-pict-window)])
  
  (defm (axis o v) a)  
  (defv (s t) (visible-range a win))
  (define (snap x d) (exact-round (* d (floor (/ x d)))))

  (let ([s (snap s d)] [t (snap t d)])
    ; the first and last tick in the range is excluded
    ; due to collision with arrow head
    (for/list ([x (in-range (+ s d) t d)])
      (tick a x #:size ts))))

(define (tick-label a x)
  (def α (angle2 (axis-dir a) (vec 1 0)))
  (def label-maker
    (cond
      [(<= α α π/4) label-top]
      [else         label-rt]))
  (label-maker (~r x #:precision 4)
               (coordinate->pt a x)))

(define (tick-labels a       ; axis
                      [d 1]  ; axis units between ticks
                      #:window [win (curve-pict-window)])                      
  (defm (axis o v) a)  
  (defv (s t) (visible-range a win))
  ; the first and last tick in the range is excluded
  ; due to collision with arrow head
  (for/draw ([x (in-range (+ s d) t d)])            
            (tick-label a x)))
  
(define (unit-label a)
  (def α (angle2 (axis-dir a) (vec 1 0)))
  (def label-maker
    (cond
      [(<= α α π/4) label-bot]
      [else         label-lft]))  
  (label-maker "1" (coordinate->pt a 1)))


  

; (require plot/private/common/axis-transform)

; (def Log   (invertible-function log exp))
; (def LogT  (make-axis-transform Log))
; (def T (apply-axis-transform LogT 1 1000))

; (define h
;  (let ()
;    (defm (invertible-function f g) T)
;    (λ (x)  (f x))))












