#lang racket/base
(require "def.rkt" "structs.rkt" "color.rkt" "pt-vec.rkt"
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

         ball-gradient    ; transition from:  white to color to black
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


(define (linear-gradient p0 p1 colors #:stops [ss #f] #:height-factor [hf 1])
  (def stops (cond [(raw-gradient? colors) (raw-gradient-color-stops colors)]
                   [else                   (color-stops colors ss)]))
  (raw-linear-gradient stops p0 p1 hf))

(define (radial-gradient p0 r0 p1 r1 colors #:stops [ss #f] #:height-factor [hf 1])
  (def stops (cond [(raw-gradient? colors) (raw-gradient-color-stops colors)]
                   [else                   (color-stops colors ss)]))
  (raw-radial-gradient stops p0 r0 p1 r1 hf))


(define (convert-gradient g P) 
  ; convert a gradient into a  linear-gradient% or a radial-gradient%
  ; P is a trans from logical coordinates to pattern coordinates
  (define (assemble-stops col-stops)
    (match col-stops
      [(raw-color-stops colors stops)
       (for/list ([c colors] [s stops])
         (list s (make-color* c)))]))
  (match g
    [(raw-linear-gradient stops p0 p1 hf)
     (defm (pt x0 y0) (P p0))
     (defm (pt x1 y1) (P p1))
     (def stops* (assemble-stops stops))
     (new linear-gradient% [x0 x0] [y0 y0] [x1 x1] [y1 y1]
          [stops stops*])]

    [(raw-radial-gradient stops p0 r0 p1 r1 hf)
     (defm (pt x0 y0) (P p0))
     (defm (pt x1 y1) (P p1))
     (def pr0 (dist (P (pt r0 0)) (P origo)))
     (def pr1 (dist (P (pt r1 0)) (P origo)))

     (def stops* (assemble-stops stops))
     (new radial-gradient%
       [x0 x0] [y0 y0] [r0 pr0]
       [x1 x1] [y1 y1] [r1 pr1]
       [stops stops*])]
    
    [_ (error 'convert-gradient)]))


(define (gradient colors [stops #f])
  (raw-gradient (color-stops colors stops)))

(define (to-linear-gradient g p0 p1 [hf 1])
  (defm (raw-gradient cs) g)
  (raw-linear-gradient cs p0 p1 hf))

(define (to-radial-gradient g p0 r0 p1 r1 [hf 1])
  (defm (raw-gradient cs) g)
  (raw-radial-gradient cs p0 r0 p1 r1 hf))

(define (ball-gradient c)
  ; this is the ball gradient from TikZ
  ; fades from white to c to black
  (def stops  (map (λ(x) (/ x 50.)) (list 0 9 18 25 50)))
  (def colors (list (color-med 0.15 "white" c)
                    (color-med 0.75 "white" c)
                    (color-med 0.70 "black" c)
                    (color-med 0.50 "black" c)
                    "black"))
  (gradient colors stops))
