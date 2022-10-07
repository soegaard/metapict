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

         find-gap-size            ; find nice distance between ticks 
         find-first-tick-in-range ; find first nice tick coordinate
         find-last-tick-in-range  ; find last nice tick coordinate
         tick-labels-in-range
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

;;  (list 's (system: (pt 0 0) (axis (pt 0 0) (vec 1 0) #f) (axis (pt 0 0) (vec 0 1) #f)))
;;  (list 'win (window -2 7 -9.5 -0.1))
;;  '(fun "(function (+ (* -1 x) -2) ]-∞,∞[ #<procedure:...ict/function.rkt:41:6>)"))
;; (fun-graph x-range #f #f y-range -9.499999999995623 -0.09999999983634256)

;; (fun-graph x-range #f #f y-range 0.09999999997671694 3.199999999953434)
;; #(struct:system: #(struct:pt 0 0) #(struct:axis #(struct:pt 0 0) #(struct:vec 1 0) #f) #(struct:axis #(struct:pt 0 0) #(struct:vec 0 1) #f))
;; #(struct:window -2 6 0.1 3.2)

(require (only-in "clipping.rkt" window-clip))
(require racket/pretty)
(define (visible-range a win)
  ; return the start and end (in logical coordinates i.e. not axis coordinates)
  ; of range in which the axis is visible.
  ; Note: If you see #f #f it means the axis is entirely outside the window.
  (defm (axis o v _) a)
  (defm (pt ox oy) o)
  (defm (window minx maxx miny maxy) win)
  ;; x-range
  (defm (pt x1 y1) (pt+ o (vec*  1000000. v)))
  (defm (pt x2 y2) (pt+ o (vec* -1000000. v)))
  (defm (list c1 c2) (window-clip minx maxx miny maxy x1 y1 x2 y2))
  (define d1 (and c1 (coordinate a (apply pt c1))))
  (define d2 (and c2 (coordinate a (apply pt c2))))
  #;(pretty-print
   (list
    (list 'a a)
    (list 'win win)
    (list 'new (list (and c1 (min d1 d2))
                     (and c2 (max d1 d2))))
    (list 'old (let-values ([(v1 v2) (old-visible-range a win)])
                 (list v1 v2)))))
  (if (and d1 d2)
      (values (min d1 d2) (max d1 d2))
      (values d1 d2)))


(define (old-visible-range a win)  
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
  (define (line p v) (curve (pt+ p (vec* 1000000. v)) -- (pt+ p (vec* -1000000. v))))
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
    ; (displayln (list s t))
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
                     #:opposite   [opposite #f]
                     #:window     [win (curve-pict-window)]
                     #:fill       [fill #f]  ; boolean or color
                     #:first-tick [s0 #f])
  (defm (axis o v l) a)  
  (defv (s t) (visible-range a win))
  (set! s0 (or s0 s))
  
  ; the first and last tick in the range is excluded
  ; due to collision with arrow head
  (for/draw ([x (in-range (+ s0 d) t d)])
    (match fill
      [#f                     (tick-label a x opposite)]
      [#t (fill-label "white" (tick-label a x opposite))]
      [c  (fill-label c       (tick-label a x opposite))])))


(define (tick-labels-in-range
         a      ; axis
         s t    ; where s<t is the interval
         [d 1]  ; axis units between ticks                     
         #:opposite   [opposite #f]
         #:window     [win (curve-pict-window)]
         #:fill       [fill #f]  ; boolean or color
         #:first-tick [s0 #f]
         #:omit       [omit '()])
  (defm (axis o v l) a)  
  (set! s0 (or s0 s))
  
  ; the first and last tick in the range is excluded
  ; due to collision with arrow head
  (for/draw ([x (in-range (+ s0 d) t d)])
    (and (not (memf (λ (y) (= x y)) omit))
         (match fill
           [#f                     (tick-label a x opposite)]
           [#t (fill-label "white" (tick-label a x opposite))]
           [c  (fill-label c       (tick-label a x opposite))]))))

(define (unit-label a)
  (def α (angle2 (axis-dir a) (vec 1 0)))
  (def label-maker
    (cond
      [(<= α α π/4) label-bot]
      [else         label-lft]))  
  (label-maker "1" (coordinate->pt a 1)))


  ;; (define gap-size (find-gap-size s t #:at-least 5))
  ;; (list 's s 't t 'gapsize gap-size 'last-tick (find-last-tick-in-range s t gap-size)))


;;;
;;; Gap size between axis ticks.
;;;

;; When the visible range is know, we need to find
;; where the ticks go. To do that, we need to find
;; an reasonable gap size.

(define (log10 x) (log x 10))

(define (find-gap-size s t #:at-least [at-least #f])
  (unless (< s t)
    (error 'find-gap-size "first argument must be smaller than second argument"))

  (define (find-base-gap-size s t)
    (cond
      [(= s t) #f]
      [(< t s) (find-gap-size t s)]
      [else    (define l (log10 (- t s)))
               (expt 10 (floor l))]))

  (define (find-better-gap-size base gaps at-least)
    ; The `base` gap size results in `gaps` gaps.
    ; We need at least `at-least` gaps.
    (for/or ([s '(1 2 4 5 10)])
      (and (>= (* s gaps) at-least)
           (/ base s))))

  (define base (find-base-gap-size s t))
  (define gaps (floor (/ (- t s) base)))

  (if at-least
      (find-better-gap-size base gaps at-least) 
      base))

(define (find-last-tick-in-range s t gap-size)
  (define t/g (/ t gap-size))
  (if (positive? t/g)
      (* (floor   t/g) gap-size)
      (* (ceiling t/g) gap-size)))

(define (find-first-tick-in-range s t gap-size)
  (define s/g (/ s gap-size))
  (if (positive? s/g)
      (* (ceiling s/g) gap-size)
      (* (floor   s/g) gap-size)))

;; (let ([s 0.18] [t 2.92])
;;   (define gap-size (find-gap-size s t #:at-least 5))
;;   (list 's s 't t 'gapsize gap-size 'last-tick (find-last-tick-in-range s t gap-size)))

;; (let ([t -0.18] [s -2.92])
;;   (define gap-size (find-gap-size s t #:at-least 5))
;;   (list 's s 't t 'gapsize gap-size 'last-tick (find-last-tick-in-range s t gap-size)))

;; (let ([s 0.18] [t 2.92])
;;   (define gap-size (find-gap-size s t #:at-least 5))
;;   (list 's s 't t 'gapsize gap-size 'first-tick (find-first-tick-in-range s t gap-size)))

;; (let ([t -0.18] [s -2.92])
;;   (define gap-size (find-gap-size s t #:at-least 5))
;;   (list 's s 't t 'gapsize gap-size 'first-tick (find-first-tick-in-range s t gap-size)))


;; (find-base-gap-size -22 54)  ; 10
;; (find-base-gap-size 22 25)   ;  1
;; (find-base-gap-size 0.2 0.9) ;  0.1

;; (find-gap-size -22 54  #:at-least 5) ;  10
;; (find-gap-size 22 25   #:at-least 5) ;   0.5
;; (find-gap-size 0.2 0.9 #:at-least 5) ;   0.1

;; (find-gap-size -22 54  #:at-least 7) ;  10
;; (find-gap-size 22 25   #:at-least 7) ;   0.25
;; (find-gap-size 0.2 0.9 #:at-least 7) ;   0.05

  

; (require plot/private/common/axis-transform)

; (def Log   (invertible-function log exp))
; (def LogT  (make-axis-transform Log))
; (def T (apply-axis-transform LogT 1 1000))

; (define h
;  (let ()
;    (defm (invertible-function f g) T)
;    (λ (x)  (f x))))
