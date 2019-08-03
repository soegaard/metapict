#lang racket/base
(require "def.rkt" "structs.rkt" "color.rkt"
         racket/format racket/match racket/class racket/draw)
;;;
;;; Color Gradients
;;;

;; A color gradient consists of a list of colors and a list of numbers
;; from 0 to 1. 


(provide gradient         ; create a color transisition
         linear-gradient  ; create a color transition in a direction
         radial-gradient  ; create a color transition between two circles

         to-linear-gradient ; give color transition a direction
         to-radial-gradient ; give color transition a center

         convert-gradient ; convert to racket/draw class gradients
         color-stops
         )

(define (color-stops colors [stops #f])
  (when stops
    (unless (andmap (λ (x) (and (number? x) (<= 0 x 1))) stops)
      (error 'new-gradient (~a "stops must be numbers between 0 and 1, got: "
                               stops))))
  (when stops
    (unless (= (length colors) (length stops))
      (error
       'new-gradient
       (~a "the color list and the stop list are not of the same length, got: "
           colors stops))))

  (def 1/n (/ (max 1 (sub1 (length colors)))))
  (def the-stops (for/list ([s (or stops (in-range 0 (+ 1 1/n) 1/n))]) s))
  (raw-color-stops colors the-stops))


(define (linear-gradient p0 p1 colors #:stops [stops #f])
  (raw-linear-gradient (color-stops colors stops) p0 p1))

(define (radial-gradient p0 r0 p1 r1 colors #:stops [stops #f])
  (raw-radial-gradient (color-stops colors stops) p0 r0 p1 r1))


(define (convert-gradient g P) 
  ; convert a gradient into a  linear-gradient% or a radial-gradient%
  ; P is a trans from logical coordinates to pattern coordinates
  (define (assemble-stops col-stops)
    (match col-stops
      [(raw-color-stops colors stops)
       (for/list ([c colors] [s stops])
         (list s (make-color* c)))]))
  (match g
    [(raw-linear-gradient stops p0 p1)
     (defm (pt x0 y0) (P p0))
     (defm (pt x1 y1) (P p1))
     (def stops* (assemble-stops stops))
     (new linear-gradient% [x0 x0] [y0 y0] [x1 x1] [y1 y1]
          [stops stops*])]

    [(raw-radial-gradient stops p0 r0 p1 r1)
     (defm (pt x0 y0) (P p0))
     (defm (pt x1 y1) (P p1))
     (def stops* (assemble-stops stops))
     (new radial-gradient%
       [x0 x0] [y0 y0] [r0 r0]
       [x1 x1] [y1 y1] [r1 r1]
       [stops stops*])]
    
    [_ (error 'convert-gradient)]))


(define (gradient colors [stops #f])
  (raw-gradient (color-stops colors stops)))

(define (to-linear-gradient g p0 p1)
  (defm (raw-gradient cs) g)
  (raw-linear-gradient cs p0 p1))

(define (to-radial-gradient g p0 r0 p1 r1)
  (defm (raw-gradient cs) g)
  (raw-radial-gradient cs p0 r0 p1 r1))

