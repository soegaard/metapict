#lang racket/base
(require (for-syntax racket/base) pict/convert racket/format)

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

