#lang racket/base
(require (for-syntax racket/base)
         pict/convert racket/format
         "parameters.rkt")
         

(define-syntax (provide-structs stx)
  (syntax-case stx ()
    [(_ id ...)
     #'(begin (provide (struct-out id) ...))]))

;;; Base types
(provide-structs arw bez curve: mat pt vec window)
;;; Paths
(provide-structs path open-path closed-path knot knot/info
                 segment type open explicit non-explicit tenscurl
                 given endpoint end-cycle)
;;; Path Joins
(provide-structs join tension-and controls-and full-join)
;;; Direction Specifier
(provide-structs curl)
;;; Accessors
(provide left-tension right-tension get-tension)
(provide-structs path-operation)
;;; Labels and their placements
(provide-structs label lft rt top bot ulft urt llft lrt cnt placement)


; The basic data types 
(struct pt     (x y)                 #:transparent) ; point
(struct vec    (x y)                 #:transparent) ; vector
(struct arw    (pt vec)              #:transparent) ; arrow 
(struct bez    (p0 p1 p2 p3)         #:transparent) ; bezier curve
(struct mat    (a b c d)             #:transparent) ; 2x2 matrix [[a b] [c d]]
(struct window (minx maxx miny maxy) #:transparent) ; window (coordinate system)
(struct curve: (closed? bezs)        #:transparent  ; a resolved curve is a list of bezs   
  #:reflection-name 'curve)


;;;
;;; Representation of paths (274, 275)
;;;

(require "list3-sequence.rkt")

;; A path is basically a list of knots, which can be open or closed.
;; This representation is meant to be internal. Users should use path: to construct paths.
(struct path (knots) #:transparent)
(struct open-path path () #:transparent
  #:property prop:sequence (λ (p) (make-3-sequence (path-knots p))))  
(struct closed-path path () #:transparent
  #:property prop:sequence (λ (p) (make-3-sequence (path-knots p))))
;; A knot consists of 
;;    p                       a point
;;    p-,p+                   the previous and the following control point
;;    left-type, right-type   control info
;; Note: a #f in p- and p+ means the control points haven't been computed yet.
(struct knot (p p- p+ left-type right-type) #:transparent)
; The first step in computing control points is to determine
; turning angles and distances to previous and posteriour knots.
(struct knot/info knot (ψ d- d+) #:transparent)
;; A SEGMENT is list of knots (with info) whose first and last knots are breakpoints.
(struct segment (knots) #:transparent
  #:property prop:sequence (λ (p) (make-3-sequence (segment-knots p))))
;; A type is one of
(struct type () #:transparent)
(struct explicit type () #:transparent); the Bezier control points have already been 
;                                      ; computed (in p- and p+).
(struct non-explicit type (τ)          ; All non-explicit control point types have a tension τ.
  #:transparent) 
(struct open non-explicit ()           ; the curve leaves the knot in the same direction 
  #:transparent)                       ; it enters, MP finds the direction
(struct tenscurl non-explicit (amount) ; the curve should leave the knot in a direction 
  #:transparent)                       ; depending  on the angle at which it enters the next knot
(struct given non-explicit (angle)     ; the curve enters/leaves (left/right) in a known angle
  #:transparent)                       ; Note: this is an absolute angle (φ and θ are relative!)
(struct endpoint type ()               ; for an open path the first endpoint do not use z- 
  #:transparent)                       ; and the last doesn't use z+.
(struct end-cycle type ()              ; temporary type: used to break cycles
  #:transparent)

;;; Basic Path Joins
(struct join ()                    #:transparent)
(struct tension-and  join (τ- τ+)  #:transparent)
(struct controls-and join (c- c+)  #:transparent)
(struct & join ()                  #:transparent)
;;; Full Join
(struct full-join join (ds- j ds+) #:transparent)

;; Note: A negative tension τ is interpreted as tension "at least abs(τ)".
;;       The default tension is 1. A tension of ∞ gives a (almost) linear curve.
;;       Tensions are always at least 

; <direction specifier> ::= <empty> | (curl <num-expr>) | <vec-expr>
(struct curl (amount) #:transparent)

; accessors
(define (left-tension k)  (non-explicit-τ (knot-left-type k)))
(define (right-tension k) (non-explicit-τ (knot-right-type k)))
(define (get-tension t) (and (non-explicit? t) (non-explicit-τ t)))

; path operations
;   the user can specify his own path-operations that
;   expands a path specification into a simpler one
(struct path-operation (handle) #:transparent)


;;; Labels
(struct label (string-or-pict pos plc) #:transparent)
;;; Label placements
(struct placement ())
(struct lft  placement()) ; left
(struct rt   placement()) ; right
(struct top  placement()) ; top
(struct bot  placement()) ; bottom
(struct ulft placement()) ; upper left
(struct urt  placement()) ; upper right
(struct llft placement()) ; lower left
(struct lrt  placement()) ; lower right
(struct cnt  placement()) ; center

;;; Nodes and Edges

; A SHAPE represents a (closed?) curve.
; It has
;  - a curve
;  - an anchor function   vec -> pt   returns a point on the shape in the given direction
;  - a  normal function   vec -> vec  returns normal (vector orthogonal to curve) pointing outwards
(struct shape (curve anchor normal) #:transparent)
(provide-structs shape)

; A NODE has 
;  - a position pos   the node is centered over pos
;  - an inner shape   the shape drawn around the text
;  - an outer shape   the shape used to place anchors (normally larger than the inner shape)
;  - label            (or #f for none)
(struct node (convert pos inner-shape outer-shape contents)
  ; convert : node -> pict   ; is called by pict-convert
  #:transparent
  #:property prop:pict-convertible (λ (v) ((node-convert v) v)))
(provide-structs node)

; An Edge represents a connection (arrow) between two nodes.
; An Edge has
; - a curve
; - an from and to node
; - direction leaving the from node (#f is auto)
; - direction entering the to node  (#f is auto)
; - an from and to arrow head (or #f for none)
; - a label (or #f for none)
; - a label direction (or #f for auto)

(struct edge (convert curve from to from-dir to-dir from-head to-head label label-direction)
  #:transparent
  #:constructor-name make-edge
  #:property prop:pict-convertible (λ (v) ((edge-convert v) v)))

(provide (except-out (struct-out edge) edge))


;;;
;;; Color Gradients
;;;

(struct raw-color-stops (colors stops) #:transparent)

; the points and radii are in logical coordinates
(struct raw-gradient (color-stops)                                     #:transparent)
(struct raw-linear-gradient raw-gradient (p0 p1 height-factor)         #:transparent) 
(struct raw-radial-gradient raw-gradient (p0 r0 p1 r1 height-factor)   #:transparent)

; The width of a, say, node times the height-factor gives the height of the node.
; This is used to adapt radial gradients to ellipses (in user space).

(provide-structs raw-color-stops raw-gradient raw-linear-gradient raw-radial-gradient)


;;;
;;; Drawable
;;;

; If a structure has the prop:drawable property, draw
; will use the function draw-convert to render it.

(define-values (prop:drawable drawable? drawable-convert)
  (make-struct-type-property 'prop:drawable))

(provide prop:drawable drawable? drawable-convert)

;;;
;;; Property: Convertible to pt 
;;;

(define-values (prop:pt-convertible pt-convertible? pt-convert)
  (make-struct-type-property 'prop:pt-convertible))

(provide prop:pt-convertible pt-convertible? pt-convert)


;;;
;;;  Domain
;;;

; A domain interval represents an interval, a subset of the real number line
; of one of the following forms:
;    i)   a<x<b      open-open
;   ii)   a<=x<b   closed-open
;  iii)   a<x<=b     open-closed
;   iv)   a<=x<=b  closed-closed

; A domain consists of a list of intervals.
; The invariant is:
;   I)  the intervals doesn't overlap
;  II)  the intervals are sorted in ascending order

(struct domain-interval (from-closed? from to to-closed?) #:transparent)
(struct domain          (intervals)                       #:transparent)
(provide-structs domain-interval domain)

;;;
;;;  Axis
;;;

;; An axis represent a an axis of a coordinate system.
;; An axis consist of an origin (point where 0 is placed) and a unit-vector,
;; which is a vector from the origo to the point where 1 is placed.
;; The coordinates of origin and unit-vector are logical coordinates.

(struct axis (origin unit-vector)
  #:property prop:drawable (λ (a) ((current-draw-axis) a))
  #:transparent)
(provide-structs axis)

;;;
;;; Coordinate System
;;;

(struct system: (origin axis1 axis2)
  #:property prop:drawable (λ (a) ((current-draw-system) a))
  #:transparent)
(provide-structs system:)

;;;
;;; Point
;;;

(struct point: (system pt)
  #:property prop:drawable       (λ (p) ((current-draw-point) p))
  #:property prop:pt-convertible (λ (p) ((current-point-to-pt-converter) p))
  #:transparent)
(provide-structs point:)


;;;
;;; Geometry
;;;                             

; Note: Rather than refer to draw-line, draw-parabola here,
;       we refer to current-draw-line, current-draw-parabola.
;       This 1) prevents a circular reference
;       and  2) allows custom renderers.

(struct line: (p q l r)
  #:reflection-name 'line
  #:property prop:drawable (λ (l) ((current-draw-line) l))
  #:transparent)

  
(provide-structs line:)
;   Here p and q are points in logical coordinates
;   and l and r are booleans.

; (line p q #t #t) represents a line through points p and q
; (line p q #f #f) represents a line segment from p to q
; (line p q #t #f) represents a ray from q through p
; (line p q #f #t) represents a ray from p through q

(struct conic: (f v e)
  #:transparent
  #:property prop:drawable
  (λ (c) ((current-draw-conic) c)))
(provide-structs conic:)
; f focus
; v vertex
; e eccentricity  (e=1 parabola, e>1 hyperbola, 0<e<1 ellipse)


(struct parabola: (f v)
  #:transparent
  #:property prop:drawable
  (λ (p) ((current-draw-parabola) p)))
(provide-structs parabola:)

; f and v are points:
;   f is the focus 
;   v is the vertex 
; The parabola consists of all points whose distances to f and v are the same

; The focal parameter a is the distance from the from the vertex to the focus.

