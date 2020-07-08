#lang racket/base

(require racket/draw math/flonum racket/format
         "pict-lite.rkt"
         (only-in srfi/1 zip unzip2)
         "angles.rkt" "arrow.rkt" "bez.rkt" "bitmap.rkt" "color.rkt" "curve.rkt" 
         "dc.rkt" "def.rkt" "device.rkt" "draw.rkt" "font.rkt" "gradient.rkt"
         "grid.rkt" "label.rkt" "node.rkt" "parameters.rkt" "path.rkt"
         "pen-and-brush.rkt" "pict.rkt" "pt-vec.rkt" "shade.rkt" "text-path.rkt"
         "shapes.rkt" "structs.rkt" "text.rkt" "trans.rkt" "trig.rkt" "window.rkt")

(provide (all-from-out
          "pict-lite.rkt"
          "bitmap.rkt" "def.rkt" "dc.rkt" "bez.rkt" "font.rkt" "text.rkt" "window.rkt"
          "path.rkt" "curve.rkt"
           "trig.rkt" "pt-vec.rkt"  "angles.rkt" "trans.rkt"
          "color.rkt" "pict.rkt" "device.rkt" "arrow.rkt" "shapes.rkt"
          "pen-and-brush.rkt" "shade.rkt" "text-path.rkt"
          "draw.rkt" "label.rkt" "shapes.rkt" "node.rkt" "parameters.rkt" "arrow.rkt"
          "grid.rkt" "gradient.rkt" "structs.rkt")
         ; macros from draw.rkt
         for/draw for*/draw
         ; macros from window.rkt
         with-window
         with-scaled-window
         ~vec)


(define (~vec v) 
  (define (t x) (text (~a x)))
  (defm (vec x y) v)
  (beside (t "(") (scale 0.5 (above (t x) (t y))) (t ")")))
