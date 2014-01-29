#lang racket

(require racket/draw  math/flonum
         (except-in pict arrow)
         (only-in srfi/1 zip unzip2)
         "def.rkt" "dc.rkt" "bez.rkt" "window.rkt"  "path.rkt" "curve.rkt"
         "structs.rkt" "trig.rkt" "pt-vec.rkt"  "angles.rkt" "trans.rkt"
         "color.rkt" "pict.rkt" "device.rkt" "arrow.rkt" "shapes.rkt" "pen-and-brush.rkt"
         "draw.rkt" "label.rkt" "shapes.rkt" "parameters.rkt")

(provide (all-from-out
          pict
          "def.rkt" "dc.rkt" "bez.rkt" "window.rkt"  "path.rkt" "curve.rkt"
          "structs.rkt" "trig.rkt" "pt-vec.rkt"  "angles.rkt" "trans.rkt"
          "color.rkt" "pict.rkt" "device.rkt" "arrow.rkt" "shapes.rkt" "pen-and-brush.rkt"
          "draw.rkt" "label.rkt" "shapes.rkt" "parameters.rkt" "arrow.rkt")
         ; macros from window.rkt
         with-window
         with-scaled-window
         above 
         beside
         ~vec)

(define above  vc-append)
(define beside hc-append)

(define (~vec v) 
  (define (t x) (text (~a x)))
  (defm (vec x y) v)
  (beside (t "(") (scale (above (t x) (t y)) 0.5) (t ")")))
