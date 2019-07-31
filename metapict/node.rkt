#lang racket/base
(require "angles.rkt" "arrow.rkt" "curve.rkt" "def.rkt" "draw.rkt" "label.rkt" 
         "path.rkt" "pt-vec.rkt" "shapes.rkt" "structs.rkt" "trans.rkt" "parameters.rkt"
         "text.rkt"
         racket/list racket/match racket/format)
(require (for-syntax syntax/parse racket/base))

(provide circle-node          ; create node shaped as a circle
         square-node          ; create node shaped as a square
         rectangle-node       ; create node shaped as a rectangle
         text-node            ; create node containing text
         draw-node-outline    ; draw node
         filled-node          ; draw filled node
         edge                 ; edge (curve/arrow) from one node to another
         anchor               ; find anchor i.e. point on the outline
         normal               ; find normal vector to the outline 
         current-node-size    ; default size for circles and half-diameter for squares
)   

; A NODE has 
;  - a position pos   the node is centered over pos
;  - a curve          the curve determines the outline of the node
;  - anchor           vec -> pt function, returns a point on the outline in the given direction
;  - normal           vector normal to the outline pointing outwards
;  - contents         a string or pict to be displayed in the node (#f means nothing)
; (struct node (pos curve anchor normal) #:transparent)

(define (anchor n v) ((shape-anchor (node-outer-shape n)) v))
(define (normal n v) ((shape-normal (node-outer-shape n)) v))

;; Defaults
; current-node-side determines the radius in circles and
; the half-diameter for squares when no size is given.
(define current-node-size (make-parameter 0.2)) ; which unit?

(define (draw-shape s)
  (draw (shape-curve s)))

(define (filldraw-shape s)
  (filldraw (shape-curve s)))

(define (draw-node-outline n)
  (draw (draw-shape (node-inner-shape n))
        #;(node-contents n)))

(define (filled-node n)
  (draw (filldraw-shape (node-inner-shape n))
        #;(node-contents n)))

; edge : node node <optionals> -> edge
;   Construct an edge struct that represents an edge from the node `from` to the node `to`.
;   When drawn an edge will draw a curve between the nodes.
;   The start and end of the edge can be marked with a head (nothing, arrowhead, others).
;   The anchor (attachment points) of the two nodes and the directions of leaving and
;   entering the nodes are determined by the anchor and normal functions of the nodes.
;   It is possible to attach an optional label to the edge.

;   If the default curve drawn between the two nodes are unsatisfactory,
;   it is possible to specify a few intermediary points on the curve. Use #:via.
(define (edge from to
              ; directions the edge is leaving/entering the from/to-node
              [from-dir #f]
              [to-dir   #f]
              ; points on the curve between the nodes
              #:via       [via      #f]  ; #f, pt or a list of pt
              ; the arrow heads used to draw arrow heads (#f means none)
              #:from-head [from-head #f]
              #:to-head   [to-head   arrow-head]
              ; instead of setting from-head and to-head one use #:arrow
              ; with one of these symbols: - -> <-> <-
              ; If used it overrids from-head and to-head
              #:arrow     [arrow     #f]
              ; a label and the direction in which to place it 
              #:label     [label-str/pict #f]
              #:label-dir [label-dir      #f]
              #:label-gap [label-gap      #f]) ; #f means auto
  ; handle via points
  (when via
    (when (pt? via)
      (set! via (list via))))
  ; handle arrow head shortcuts
  (when arrow
    (set!-values (from-head to-head) 
    (match arrow
      ['-   (values #f #f)]
      ['->  (values #f arrow-head)]
      ['<-> (values arrow-head arrow-head)]
      ['<-  (values arrow-head #f)]
      [_ (error 'edge (~a "expected an arrow shortcut (one of - -> <-> ->), got " arrow))])))  
  ; the two nodes
  (def n1 from)
  (def n2 to)
  ; position of node centers
  (def p1 (node-pos n1))
  (def p2 (node-pos n2))
  ; direct direction beween nodes
  (def v  (pt- p2 p1))
  ; leaving and entering directions
  (def use-mid? #t)
  (define-values (d1 d2)
    (match* (from-dir to-dir)
      [(#f #f)
       ; no directions given - use the direct direction
       (values v (vec* -1 v))]
      [(from-dir #f)
       (set! use-mid? #f)
       ; only the leaving direction is given
       (values from-dir from-dir)]
      [(#f to-dir)
       (set! use-mid? #f)
       ; only the entering direction is given
       (values (vec* -1 to-dir) (vec* -1 to-dir))]
      [(from-dir to-dir)
       (values from-dir (vec* -1 to-dir))]))
  ; anchors (points on the outline of the two nodes) to attach the edge
  (def a1 (anchor n1 d1))
  (def a2 (anchor n2 d2))
  ; If the node is circle shaped we could use the directions d1 and d2 as is.
  ; Given other shapes, we need to use a normal instead.
  ; Hmmm - for now we just use the directions directly - looks better.
  ; Maybe only for rectangualr nodes?
  ;(def v1 (normal n1 d1))
  ;(def v2 (vec* -1 (normal n2 d2)))
  (def v1 d1)
  (def v2 (vec* -1 d2))
  ; curve connecting the two anchor points
  (def c (cond 
           [via
            (curve* (append (list a1 v1 ..)
                            (append-map (λ (v) (list v ..)) via)
                            (list v2 a2)))]
           [(and use-mid? (vec= v1 (vec* -1 v2)))  ; the special case 
            (def m (pt+ p1 (vec* 0.5 v))) ; "mid" point to achieve a prettier path
            (curve a1 v1 .. m .. v2 a2)]
           [else                          ; the general case
            (curve a1 v1 .. v2 a2)]))
  (def gap (or label-gap (current-label-gap)))
  (when (string? label-str/pict)
    (set! label-str/pict (text label-str/pict)))
  (def l (and label-str/pict
              (let ()
                (def t (/ (curve-length c) 2)) ; time of mid point
                (def p (point-of c t))         ; mid point
                (def d (direction-of c t))
                (def n (rot-90 d))
                (def pos (pt+ p (vec* (/ (* gap 1.1) (norm n)) n)))
              (label label-str/pict pos (cnt)))))
           
  ;convert : edge -> pict
  (define (convert e)    
    (match* (from-head to-head)
      [(#f #f) (draw c l)]
      [(fh #f) (draw (draw-arrow c #:head #f #:tail fh) l)]
      [(fh th) (draw (draw-arrow c #:head th #:tail fh) l)]
      [(#f th) (draw (draw-arrow c #:head th #:tail #f) l)]))
  ; 
  (make-edge convert c from to
             d1 d2
             from-head to-head label label-dir))

; contents-dimension : string-or-label -> integer integer
;   return both width and height of the text-or-label
(define (contents-dimension t)
  (match t
    [(or "" #f)
     (values 0 0)]
    [_ (def l (or (and (label? t) t)
                  (label-cnt t origo)))  
       (def outline (label-bbox l))  ; curve consisting of 4 bezs  
       ; - corners and diameter of bounding box
       (def c0   (point-of outline 0))
       (def c1   (point-of outline 1))
       (def c2   (point-of outline 2))
       (def c3   (point-of outline 3))
       (def diam (max (dist c0 c1) (dist c0 c2)))
       (def cs   (list c0 c1 c2 c3))
       (def xs   (map pt-x cs))
       (def ys   (map pt-y cs))
       ; - tight width and height of text
       (def w    (- (apply max xs) (apply min xs)))
       (def h    (- (apply max ys) (apply min ys)))
       (values w h)]))

(define-syntax (define-node-constructor stx)
  (syntax-parse stx
    [(_define-node-constructor  (constructor-name ([keyword arg-name arg-default] ...) make))
     (syntax/loc stx
       (define (constructor-name
                [t #f] ; contents (string or label) or false for no contents
                ;; The common arguments for all node constructors
                #:at        [center    #f]    ; position of node
                #:direction [direction #f]    ; direction of node relative to center
                ; inner separation (padding)
                #:inner-sep  [inner-sep  #f]  ; inner space between text and path
                #:inner-xsep [inner-xsep #f]  ; overrides #:inner-sep
                #:inner-ysep [inner-ysep #f]  ; overrides #:inner-sep
                ; outer separation (margin)
                #:outer-sep  [outer-sep  #f]  ; outer space between path and outside
                #:outer-xsep [outer-xsep #f]  ; overrides #:outer-sep
                #:outer-ysep [outer-ysep #f]
                ; minimum sizes (including inner separation, excluding outer)
                #:min-size   [minimum-size   #f]
                #:min-width  [minimum-width  #f] ; overrides mininum-size
                #:min-height [minimum-height #f] ; overrides minimum-size
                ; placement (alternativ to #:at)
                ;  - the following has no effect unless center is #f
                #:below     [below     #f]
                #:above     [above     #f]
                #:right-of  [right-of  #f]
                #:left-of   [left-of   #f]
                ;; The special keyword arguments
                (~@ keyword [arg-name arg-default]) ...)
         ; distance to neighbours
         (def dx (or (current-neighbour-distance-x) (current-neighbour-distance) 1))
         (def dy (or (current-neighbour-distance-x) (current-neighbour-distance) 1))
         (define dist-below (vec* dy south))
         (define dist-above (vec* dy north))
         (define dist-left  (vec* dx west))
         (define dist-right (vec* dx east))
         ; initial placement
         (define-values (dir pos)
           (cond
             [below    (values down      (pt+ (or center (anchor below    down))  dist-below))]
             [above    (values up        (pt+ (or center (anchor above    up))    dist-above))]
             [right-of (values right     (pt+ (or center (anchor right-of right)) dist-right))]
             [left-of  (values left      (pt+ (or center (anchor left-of  left))  dist-left))]
             [else     (values direction (or center origo))])) ; todo
         ; inner separation
         (define i-xsep (or inner-xsep inner-sep (current-inner-separation)))
         (define i-ysep (or inner-xsep inner-sep (current-inner-separation)))
         ; outer separation
         (define o-xsep (or outer-xsep outer-sep (current-outer-separation)))
         (define o-ysep (or outer-xsep outer-sep (current-outer-separation)))

         ; text dimension
         (define-values (w h) (contents-dimension t))
         ; handle minimum width and height
         (def min-width (or minimum-width minimum-size))
         (when min-width
           (unless (>= (+ w i-xsep) min-width)
             ; increase the inner x-separation if needed
             (set! i-xsep (max 0 (/ (- min-width w) 2.)))))
         ; handle minimum height
         (def min-height (or minimum-height minimum-size))
         (when min-height
           (unless (>= (+ h i-ysep) min-height)
             ; increase the inner y-separation if needed
             (set! i-ysep (max 0 (/ (- min-height h) 2.0)))))
         ; Notes: At this point
         ;  - minimum height and width have been turned into separations
         ;  - placements have been turned into a position and a direction
         ;  - inner and outer separations have been split into x and y separations
         (make #:contents t
               #:at pos
               #:direction dir
               #:inner-x-separation i-xsep
               #:inner-y-separation i-ysep
               #:outer-x-separation o-xsep
               #:outer-y-separation o-ysep
               (~@ keyword arg-name) ...)))]))
         

(define-node-constructor (text-node () make-text-node))
(define (make-text-node #:contents t
                        #:at pos
                        #:direction dir
                        #:inner-x-separation i-xsep
                        #:inner-y-separation i-ysep
                        #:outer-x-separation o-xsep
                        #:outer-y-separation o-ysep)
  (make-node-helper 'text t pos dir i-xsep i-ysep o-xsep o-ysep))

(define-node-constructor (rectangle-node () make-rectangle-node))
(define (make-rectangle-node #:contents t
                             #:at pos
                             #:direction dir
                             #:inner-x-separation i-xsep
                             #:inner-y-separation i-ysep
                             #:outer-x-separation o-xsep
                             #:outer-y-separation o-ysep)
  (make-node-helper 'rectangle t pos dir i-xsep i-ysep o-xsep o-ysep))

(define-node-constructor (circle-node () make-circle-node))
(define (make-circle-node #:contents t
                          #:at pos
                          #:direction dir
                          #:inner-x-separation i-xsep  
                          #:inner-y-separation i-ysep
                          #:outer-x-separation o-xsep
                          #:outer-y-separation o-ysep)
  (make-node-helper 'circle t pos dir i-xsep i-ysep o-xsep o-ysep))


; type = 'text      :  node centered at point p
; type = 'rectangle :  text with rectangular border
(define (make-node-helper type t p dir i-xsep i-ysep o-xsep o-ysep)
  ; dir=#f    means node is centered at p
  ; dir=down  means the bottom of the bounding box touches p
  (let again ([t t] [p p] [dir dir] [first? #t])
    ;; 1. We need to figure out the dimensions of the contents t.
    ; - first we make a label centered at p (in order to figure out sizes)
    (def l       (label-cnt (or t " ") p)) ; create label from string
    (define-values (w h) (if t (contents-dimension l) (values 0 0)))

    ;; 2. Shapes
    ; - the inner shape is the shape drawn around the text
    (define make-shape  (case type
                          [(text rectangle) rectangle-shape]
                          [(circle)         circle-shape]
                          [else (error)]))
    (define inner-shape (make-shape #:center p
                                    #:width  (+ w (* 2 i-xsep))
                                    #:height (+ h (* 2 i-ysep))))

    ; - the outer shape is the shape used for anchors (often there is space between)
    (define outer-shape  (make-shape #:center p
                                     #:width  (+ w (* 2 (+ i-xsep o-xsep)))
                                     #:height (+ h (* 2 (+ i-ysep o-ysep)))))
        
    ;; 3. Drawing
    (define (convert n) ; node ->pict
      (draw (case type
              [(text)                #f]
              [(rectangle circle)    (draw-shape inner-shape)]
              [else #f])
            l))
    ;; 4. If the direction  dir  is given the final center position is not p,
    ;     but another position q, sutch that a text centered at q will have
    ;     an outline through p (and qp parallel to dir).
    ;       - if first? is #t then the final center has been calculated
    ;       - otherwise calculate q and redo the above calculation
    (cond [(or (not first?) (not dir))
           (node convert p inner-shape outer-shape l)]
          [else
           ; find a center q such that a text node centered at q will
           ; have an outline through p (and qp is parallel to dir)
           (def q ((shape-anchor inner-shape) dir))
           (again t q down #f)])))

(define (circle-shape #:center [center #f] #:width [width #f] #:height [height #f])
  (def radius (max width height))
  (def r (or radius 1))
  (def p (or center (pt 0 0)))
  (define (anchor v) (pt+ p (vec* (/ r (norm v)) v)))
  (define (normal v) (vec* (/ 1 (norm v)) v))
  (shape (circle p r) anchor normal))


(define (square p r)
  (def -r (- r))
  (shifted p
           (curve (pt -r -r) --
                  (pt  r -r) --
                  (pt  r  r) --
                  (pt -r  r) -- cycle)))

(define (rectangle-shape #:center [center #f] #:width [width #f] #:height [height #f])
  (def w (or width  (* (current-node-size) 2)))
  (def h (or height (* (current-node-size) 2)))
  (def w/2 (/ w 2))
  (def h/2 (/ h 2))
  (def p (or center (pt 0 0)))
  (def α1 (atan (/ h/2 w/2)))
  (def α2 (- π α1))
  (def α3 (+ π (- π α2)))
  (def α4 (- 2π α1))
  (define (~ x y) (<= (abs (- x y)) 0.001))
  (define (normal v) 
      (def α (flmod (angle v) 2π))
      (cond
        [(~ α α1)     (vec+ up   right)]
        [(~ α α2)     (vec+ up   left)]
        [(~ α α3)     (vec+ down left)]
        [(~ α α4)     (vec+ down right)]
        [(<=  0 α α1) right]
        [(<= α1 α α2)    up]
        [(<= α2 α α3)  left]
        [(<= α3 α α4)  down]
        [else         right]))
  (define (anchor v)
    (def α (flmod (angle v) 2π))
    (def a
      (pt+ p
         (cond [(<=    0 α  α1)
                ; right edge, so x=w/2
                (vec w/2 (* w/2 (tan α)))]
               [(<=  α1 α α2)
                ; upper edge, so y=h/2
                (vec (* h/2 (tan (- π/2 α)))  h/2)]
               [(<= α2 α α3)
                ; left edge, so x=-w/2
                (vec (- w/2) (* w/2 (tan (- π α))))]
               [(<= α3 α α4)
                ; lower edge, so y=-h/2
                (vec (* (- h/2) (tan (- 3π/2 α))) (- h/2))]
               [else ; (<= 7π/4 α)
                (vec w/2 (* (- w/2) (tan (- 2π α))))])))
    a)
  (shape (rectangle (pt+ p (vec w/2 h/2)) (pt- p (vec w/2 h/2))) anchor normal))


(define (flmod n d)
  (- n (* d (floor (/ n d)))))

(define (square-shape #:center [center #f] #:radius [radius #f])
  (def r (or radius 1))
  (def p (or center (pt 0 0)))
  (define (normal v) 
      (def α (flmod (angle v) 2π))
      (cond [(<=    0 α  π/4) right]
            [(<=  π/4 α 3π/4) up]
            [(<= 3π/4 α 5π/4) left]
            [(<= 5π/4 α 7π/4) down]
            [else             right]))
  (define (anchor v)
    (def α (flmod (angle v) 2π))
      (cond [(<=    0 α  π/4) right]
            [(<=  π/4 α 3π/4) up]
            [(<= 3π/4 α 5π/4) left]
            [(<= 5π/4 α 7π/4) down]
            [else             right])
    (pt+ p (vec* r (normal v))))
  (shape (square p r) anchor normal))

(define (square-node [contents #f] #:at [p (pt 0 0)] #:radius [radius #f])
  (def r (or radius (current-node-size)))
  (def s (square-shape #:center p #:radius r))
  ; todo: inner and outer shape
  (node draw-node-outline p s s contents))



;;; TESTING

; Test file:  tests/test-node.rkt

#;(
;(require "main.rkt")
    
(ahangle         45)       ; default head angle 45 degrees
(ahflankangle    0)        ; default "curvature" of flank (in degrees)
(ahtailcurvature 0)        ; default "curvature" of the back  todo!
(ahratio         1)
    
(define n1 (circle-node (pt 0 0) .1))
(define n2 (circle-node (pt 1 0) .1))
(define n3 (square-node (pt 0 1) .1))
(define n4 (circle-node (pt 1 1) .1))
    
(margin 5
        (scale 4 (draw (draw-node n1)
                       (draw-node n2)
                       (draw-node n3)
                       (filled-node n4)
                       (draw-edge n1 n2)
                       (draw-edge n1 n3 west west)
                       (draw-edge n1 n4))))
)


