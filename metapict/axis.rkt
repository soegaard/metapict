#lang racket/base
; TODO: different types of ticks (one sided, slantes etc)
; TODO: tick labels need to use tick size to give a better placement

(require racket/list racket/match racket/math racket/format         
         "metapict.rkt" "parameters.rkt")

; An axis consist of an origin (point where 0 is placed) and a unit-vector,
; which is a vector from the origo to the point where 1 is placed.
; The coordinates of origin and unit-vector are logical coordinates.

; (struct axis   (origin unit-vector label) #:transparent)
; (struct system (axis1 axis2)              #:transparent)
; (struct point  (system pt)                #:transparent)
; (struct axis-tick (axis x size up? down?) #:transparent)

(provide (all-defined-out))

(provide axis          ; make new axis given position of origo and direction
         new-axis      ; 
         axis-dir      ; direction vector (logical coordinates)
         axis-origin   ; position of 0    (logical coordinates)
         visible-range ; two values: start and end in axis ordinates
         draw-axis
         coordinate->pt ; axis and xa to logical pt
         tick-center    ; given axis and x to position of tick center
         tick           ; given axis and x to curve of a tick
         ticks          ; produce list of tick curves in visible range
         tick-ordinates ; produce list of ordinates of the ticks
         tick-label     ; axis x to label next to tick
         tick-labels    ; 
         unit-label
         
         horizontal-axis-range ; axis win, returns range in axis coordinates
         vertical-axis-range   ; axis win, returns range in axis coordinates
         )

(define (new-axis origin unit-vector [label #f])
  (axis origin unit-vector label))

(define (axis-dir a)
  (defm (axis o v l) a)
  v)

(define (coordinate a p)
  ; given a point p along the axis a, return the coordinate of p
  ; p is given in logical coordinates
  
  (defm (axis o v l) a)
  ; axis unit size (in logical units)
  (def u (norm v))
  ; number of units 
  (def c (/ (dist o p) u))
  (if (< (angle2 v (pt- p o)) π/2)
      c
      (- c)))

(define (visible-range a win)  
  ; return the start and end (in logical coordinates i.e. not axis coordinates)
  ; of range in which the axis is visible.
  ; Note: If you see #f #f it means the axis is entirely outside the window.  
  (defm (axis o v _) a)
  (defm (window minx maxx miny maxy) win)
    
  ; clipping box
  (def left-border  (curve (pt minx miny) -- (pt minx maxy)))
  (def right-border (curve (pt maxx miny) -- (pt maxx maxy)))
  (def lower-border (curve (pt minx miny) -- (pt maxx miny)))
  (def upper-border (curve (pt minx maxy) -- (pt maxx maxy)))
  ; find intersection points of axis and clipping box
  (define (line p v) (curve (pt+ p (vec* 1000000 v)) -- (pt+ p (vec* -1000000 v))))
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

(define (horizontal-axis? a)
  (defm (axis o  v _) a)
  (defm (vec  vx vy)  v)
  (zero? vy))

(define (vertical-axis? a)
  (defm (axis o  v _) a)
  (defm (vec  vx vy)  v)
  (zero? vx))

(define (horizontal-axis-range a win)  
  ; Given a horizontal axis a, compute the range in which the window falls.
  ; Note: This returns values even if the axis doesn't intersecti the axis.
  (defm (window minx maxx miny maxy) win)
  (defm (axis o  v _) a)
  (defm (pt   ox oy)  o)
  (defm (vec  vx vy)  v)

  ; The windows has an x-range from minx to maxx in logical coordinates.
  ; We need to translate this into axis coordinates.
  (def from (/ (- minx ox) vx))
  (def to   (/ (- maxx ox) vx))
  (values from to))

(define (vertical-axis-range a win)  
  ; Given a vertical axis a, compute the range in which the window falls.
  ; Note: This returns values even if the axis doesn't intersecti the axis.
  (defm (window minx maxx miny maxy) win)
  (defm (axis o  v _) a)
  (defm (pt   ox oy)  o)
  (defm (vec  vx vy)  v)

  ; The windows has an y-range from miny to maxy in logical coordinates.
  ; We need to translate this into axis coordinates.
  (def from (/ (- miny oy) vy))
  (def to   (/ (- maxy oy) vy))
  (values from to))
  

; The axis is drawn as a line ending in an arrow.
; The axis is conceptually infinite, so we need a logical window
; so we can find the placement of the arrow.

(define (draw-axis a #:window [win (curve-pict-window)] #:fill-label [fill #f])
  (defv (xmin xmax) (visible-range a win))
  (define (coord x) (coordinate->pt a x))
  (def T    (current-curve-transformation))
  (def from (T (coord xmin)))
  (def to   (T (coord xmax)))
  (draw (with-device-window
          (parameterize ([ahlength (ypx 8)])
            (draw-arrow (curve from -- to))))
        (and (axis-label a)
             (draw-axis-label a xmax #f #:text (axis-label a) #:fill fill #:offset -8))))

(current-draw-axis draw-axis)

(define (coordinate->pt a x)
  ; given an axis a and a coordinate x in axis units,
  ; return the corresponding point in logical units
  (defm (axis o v l) a)
  (pt+ o (vec* x v)))

(define (tick-center a x)
  (coordinate->pt a x))


;; (define (tick axis x
;;               #:size [s (get current-tick-size)])
;;   (def p (tick-center axis x))
;;   (def v (vec* s (unit-vec (rot90 (axis-dir axis)))))  
;;   (curve (pt+ p v) -- (pt- p v)))

(define (tick axis x #:size [s #f] #:up? [up? #t] #:down? [down? #t])
  (axis-tick axis x s up? down?))

(define (draw-axis-tick at)
  (defm (axis-tick a x s up? down?) at)
  ;; Get current tick size, if needed.
  (unless s (set! s (get current-tick-size)))
  ; The start end points are in logical coordinates,
  ; but we need to work in the device window to get the right size.

  ;; First, find the relevant locations in logical coordinates.
  (def p (tick-center a x))      
  (def q (pt+ p (axis-dir a))) ; now vector pq is parallel to the axis

  ;; Second, convert to device coordinates.
  (define (coord x) (coordinate->pt a x))
  (def T (current-curve-transformation))
  (def P (T p))                  ; on axis
  (def Q (T q))

  ;; Third, new points in device coordinates
  (def V (unit-vec (pt- Q P)))
  (def U (pt+ P (vec*    s  (rot90 V)))) ; above axis
  (def D (pt+ P (vec* (- s) (rot90 V)))) ; below axis
  ;; Fourth, draw
  (with-device-window
    (draw (match (list up? down?)
            [(list #t #t)  (curve U -- D)]
            [(list #f #t)  (curve P -- D)]
            [(list #t #f)  (curve U -- P)]
            [(list #f #f)  (curve P -- P)]))))

(current-draw-axis-tick draw-axis-tick)

(define (ticks a      ; axis
               [d 1]  ; axis units between ticks
               #:size        [ts  (get current-tick-size)]
               #:window      [win (curve-pict-window)]
               #:last-tick?  [last?  #f]
               #:first-tick? [first? #f]
               #:up?         [up? #t]
               #:down?       [down? #t])
  ; An arrow head makes the last tick look odd,
  ; the default is to omit it.  
  (def xs (tick-ordinates a d #:window win #:last-tick? last? #:first-tick? first?))
  (for/list ([x (in-list xs)])
    (tick a x #:size ts #:up? up? #:down? down?)))

(define (tick-ordinates a      ; axis
                        [d 1]  ; axis units between ticks
                        #:window      [win (curve-pict-window)]
                        #:last-tick?  [last? #f]
                        #:first-tick? [first? #f])
  ; An arrow head makes the last tick look odd,
  ; the default is to omit it.  
  
  (defm (axis o v l) a)  
  (defv (s t)
    ; Note: If the axis is outside the window, visible-range returns #f #f.
    ;       We therefore special case the horizontal and vertical case.
    (cond [(horizontal-axis? a) (horizontal-axis-range a win)]
          [(vertical-axis?   a) (vertical-axis-range   a win)]
          [else                 (visible-range         a win)]))
  (define (snap x d)
    (exact-round (* d (floor (/ x d)))))

  (let ([s (snap s d)] [t (snap t d)])
    (displayln (list s t))
    (when (= s t) (set! t (+ s (* 10 d))))
    ; the first and last tick in the range is excluded
    ; due to collision with arrow head
    (def xs (for/list ([x (in-range (+ s d) (+ t d) d)])
              x))
    (cond
      [(and last? first)  xs]
      [(empty? xs)        '()]
      [(empty? (rest xs)) '()]
      [last?              (rest xs)]
      [first?             (reverse (rest (reverse xs)))]
      [else               (reverse (rest (reverse (rest xs))))])))

(define (draw-axis-label a x [opposite #f] 
                    #:offset [offset    0]  ; in device coordinates
                    #:text   [text     #f]
                    #:fill   [fill     #f]) ; boolean or color
  (def k (if opposite -1 1))
  (def α (angle2 (vec 1 0) (axis-dir a)))
  (def label-maker     (cond [(<= -π/4 α π/4) label-top]
                             [else            label-rt]))
  (def opp-label-maker (cond [(<= -π/4 α π/4) label-bot]
                             [else            label-lft]))
  (def off             (cond [(<= -π/4 α π/4) (vec offset -4)]
                             [else            (vec 4 (- offset))])) ; device y opposite
  (def maker (if opposite opp-label-maker label-maker))
  (def T     (current-curve-transformation))
  (def pos   (pt+ (T (coordinate->pt a x)) (vec* k off)))
  (def l     (maker (or text (~r x #:precision 4)) pos))
  (with-device-window
    (draw l)))

(define (tick-label a x      [opposite #f] 
                    #:text   [text     #f]
                    #:fill   [fill     #f]) ; boolean or color
  (def α (angle2 (axis-dir a) (vec 1 0)))
  (def label-maker     (cond [(<= α α π/4) label-top]
                             [else         label-rt]))
  (def opp-label-maker (cond [(<= α α π/4) label-bot]
                             [else         label-lft]))
  (def maker (if opposite opp-label-maker label-maker))
  (def l     (maker (or text (~r x #:precision 4))
                    (coordinate->pt a x)))
  (match fill
    [#f                     l]
    [#t (fill-label "white" l)]
    [c  (fill-label c       l)]))

(define (tick-labels a       ; axis
                      [d 1]  ; axis units between ticks
                      #:window [win (curve-pict-window)]
                      #:fill   [fill #f])  ; boolean or color
  (defm (axis o v l) a)  
  (defv (s t) (visible-range a win))
  ; the first and last tick in the range is excluded
  ; due to collision with arrow head
  (for/draw ([x (in-range (+ s d) t d)])
    (match fill
      [#f                     (tick-label a x)]
      [#t (fill-label "white" (tick-label a x))]
      [c  (fill-label c       (tick-label a x))])))
  
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
