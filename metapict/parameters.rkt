#lang racket/base
;;; Global parameters
(provide (all-defined-out))

(require "def.rkt")
(require (only-in racket/draw make-font))


;;; Helpers

(define (thunk? v)
  (and (procedure? v)
       (zero? (procedure-arity v))))

(define (get parameter)
  (def v (parameter))
  (if (thunk? v) (v) v))

;;; Device

(def current-device-post-transformation (make-parameter #f))

;;; Label
(def current-label-font (make-parameter (make-font)))
(def current-label-gap  (make-parameter 0.15))
(def label-offset       (make-parameter 3)) ; default in MetaPost is 3bp

;;; Font
;; Note: It is not possible to keep track of the font in the drawing context.
;;       The plain-text needs access to the font in order to determine the
;;       ascent and descent of the pict before returning a pict.
;;       And at that time the dc is not available. 
(def current-font      (make-parameter (make-font)))


;;; Brush
; 
(def use-default-brush? (make-parameter #t))
(def colorizer (make-parameter (λ x (error 'colorizer "internal error: parameter wasn't set"))))


;;; Arrows

; There are three colors used to draw an arrow, one for each of the:
; stem, outline and filling the arrow head.
; There is a parameter for each. If the specific color is #f,
; then the value of current-arrow-color is used.
; If that too is #f, then the current pen and brush are used.

(def current-arrow-color              (make-parameter #f))
(def current-arrow-stem-color         (make-parameter #f))
(def current-arrow-head-color         (make-parameter #f))
(def current-arrow-head-outline-color (make-parameter #f))

(def current-arrow-head               (make-parameter #f))  ; #f means arrow-head
(def current-arrow-tail               (make-parameter #f))  ; #f means none

; The stem and heads curves are normally drawn by draw and filldraw respectively.
; These parameters can be used to override this.
(def current-draw-arrow-stem          (make-parameter #f)) ; #f = draw
(def current-draw-arrow-head          (make-parameter #f)) ; #f = filldraw
(def current-draw-arrow-head-outline  (make-parameter #f)) ; #f = draw

;;; Nodes

(def current-node-minimum-size   (make-parameter #f))
(def current-node-minimum-width  (make-parameter #f))
(def current-node-minimum-height (make-parameter #f))

(def current-node-shade (make-parameter #f))

(def current-inner-separation (make-parameter 0.05)) ; separation space between text and curve drawn
(def current-outer-separation (make-parameter 0.05))  ; separation space between curve and outside
; Note: In TikZ the default outer separation is half the line width
;       When the path is drawn, this will make the anchors lie exactly on the "outside border"
;       of the path (and not on the path itself).

;; When placing a node relative to another we need some space between the nodes.
;; I.e. these parameters are used with #:above, #:below, etc are used in (node ...).
(def current-neighbour-distance-x (make-parameter #f)) ; #f means use current-neighbour-distance
(def current-neighbour-distance-y (make-parameter #f))
(def current-neighbour-distance   (make-parameter 1.))

(def current-ring-gap   (make-parameter 0.15))
(def current-ring-gap-x (make-parameter #f))
(def current-ring-gap-y (make-parameter #f))

;;; Shadings
;; The available shaing types are:
;;      #f   none
;;   'axis   linear gradient (parallel with x-axis when angle=0)
;; 'radial   radial gradient (gradient center is center of node)
;    'ball   radial gradient (gradient center is ?? slightly of the center
(def current-shading          (make-parameter #f))
(def current-shading-angle    (make-parameter 0))    ; rotates shading (degrees)
(def current-shading-gradient (make-parameter (list "gray" "white")))


;;;
;;; Edges
;;;

(def current-incoming-edge-size (make-parameter 0.15))
(def current-outgoing-edge-size (make-parameter 0.15))
(def current-edge-color         (make-parameter #f))  ; #f means use current pen and brush

(def current-test-value (make-parameter #f))

(def current-edge-arrow-head (make-parameter #f)) ; #f means arrow-head

;;;
;;; Domains
;;;

(def current-domain-color (make-parameter #f)) ; #f: use current pen and brush

;;;
;;; Axis and Ticks
;;;

(def current-tick-size (make-parameter (λ() 1))) ; 

(def current-draw-axis (make-parameter #f))


;;;
;;; Coordinate System 
;;;

(def current-draw-system           (make-parameter #f))
(def current-draw-point            (make-parameter #f))
(def current-point-to-pt-converter (make-parameter #f))


;;;
;;; Drawable
;;;

(def current-draw-line   (make-parameter #f)) ; set in geometry.rkt
(def current-draw-circle (make-parameter #f)) ; set in geometry.rkt
(def current-draw-parameterization (make-parameter #f)) 
(def current-draw-conic     (make-parameter #f))
(def current-draw-parabola  (make-parameter #f))
(def current-draw-ellipse   (make-parameter #f))
(def current-draw-hyperbola (make-parameter #f))
