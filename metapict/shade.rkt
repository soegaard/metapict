#lang racket/base

;;;
;;; Shadings
;;;

;; The process of applying a gradient to node is called shading.

;; The available shaing types are:
;;      #f   none
;;   'axis   linear gradient (parallel with x-axis when angle=0)
;; 'radial   radial gradient (gradient center is center of node)
;    'ball   radial gradient (gradient center is ?? slightly of the center

;; The shading is controlled by the parameters
;;    current-shading           #f, axis, radial or ball
;;    current-shading-angle     angle in degrees used to rotate shading
;;    current-shading-gradient  a gradient structure that represents the color transition
;; defined in "parameters.rkt".

(provide  axis-shade  ; uses axis shading to draw node 
          )


(require "def.rkt" "gradient.rkt" "node.rkt" "pt-vec.rkt" "color.rkt"
         "parameters.rkt" "pict.rkt" "draw.rkt")


(define (axis-shade n
                    #:shade-angle  [angle 0]    ; in degrees
                    #:shade-colors [colors #f]) ; #f, color list or gradient
  (def p0 (anchor n down))
  (def p1 (anchor n up))
  ; (def g (linear-gradient p0 p1 colors #f))
  (def g  (let ([g (current-shading-gradient)])
            (if (list? g)
                (gradient g)
                g)))
  (def lg (to-linear-gradient g p0 p1))
  (brushgradient lg
                 (draw n)))



