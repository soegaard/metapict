#lang racket/base

(require racket/draw math/flonum racket/format
         "pict-lite.rkt"
         (only-in srfi/1 zip unzip2)
         "angles.rkt" "arrow.rkt" "bez.rkt" "bitmap.rkt" "color.rkt" "curve.rkt" 
         "dc.rkt" "def.rkt" "device.rkt" "draw.rkt" "grid.rkt" "label.rkt"
         "node.rkt" "parameters.rkt" "path.rkt" "pen-and-brush.rkt" "pict.rkt" "pt-vec.rkt"
         "shapes.rkt" "structs.rkt" "trans.rkt" "trig.rkt" "window.rkt")

(provide (all-from-out
          "pict-lite.rkt"
          "bitmap.rkt" "def.rkt" "dc.rkt" "bez.rkt" "window.rkt"  "path.rkt" "curve.rkt"
          "structs.rkt" "trig.rkt" "pt-vec.rkt"  "angles.rkt" "trans.rkt"
          "color.rkt" "pict.rkt" "device.rkt" "arrow.rkt" "shapes.rkt" "pen-and-brush.rkt"
          "draw.rkt" "label.rkt" "shapes.rkt" "node.rkt" "parameters.rkt" "arrow.rkt" "grid.rkt")
         ; macros from draw.rkt
         for/draw for*/draw
         ; macros from window.rkt
         with-window
         with-scaled-window
         above  above*
         beside beside*
         ~vec)

(define above  vc-append)
(define beside hc-append)
(define (beside* ps) (apply beside ps))
(define (above* ps)  (apply above ps))

(define (~vec v) 
  (define (t x) (text (~a x)))
  (defm (vec x y) v)
  (beside (t "(") (scale 0.5 (above (t x) (t y))) (t ")")))
