#lang racket/base
;;; Global parameters
(provide (all-defined-out))

(require "def.rkt")

;;; Label
(def current-font-size  (make-parameter 10)) ; default in MetaPost is 10bp
(def current-label-font (make-parameter '("Gill Sans" . swiss)))
(def label-offset       (make-parameter 3)) ; default in MetaPost is 3bp

;;; Brush
; 
(def use-default-brush? (make-parameter #t))
(def colorizer (make-parameter (λ x (error 'colorizer "internal error: parameter wasn't set"))))
