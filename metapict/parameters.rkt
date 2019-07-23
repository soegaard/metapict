#lang racket/base
;;; Global parameters
(provide (all-defined-out))

(require "def.rkt")
(require (only-in racket/draw make-font))

;;; Label 
(def current-label-text-style (make-parameter 'default))
(def current-label-text-size  (make-parameter 10))       ; default in MetaPost is 10bp (racket 12)
(def current-label-text-angle (make-parameter 0.))       ; radians 
(def current-label-gap        (make-parameter 0.2))
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

;;; Nodes
(def current-inner-separation (make-parameter 0.05)) ; separation space between text and curve drawn
(def current-outer-separation (make-parameter 0.0))  ; separation space between curve and outside
