#lang racket/base
;;;
;;;  i) Transformation from logical to device coordinates
;;; ii) px, xpx and ypx:  compute logical size of a pixels

(provide curve-pict-width                  
         curve-pict-height
         curve-pict-window
         set-curve-pict-size
         current-curve-transformation
         stdtrans
         px xpx ypx
         devpt)

(require "def.rkt" "pict-lite.rkt" "pt-vec.rkt" "structs.rkt" "trans.rkt"
         (for-syntax syntax/parse racket/base)
         racket/list racket/math)


(def curve-pict-width  (make-parameter 100))
(def curve-pict-height (make-parameter 100))
(def curve-pict-window (make-parameter ; (window -1 1 -1 1)
                        (window -1.1 1.1 -1.1 1.1)))

(define (set-curve-pict-size . xs)
  (defm (or (list w h)
            (and (list (? pict?)) 
                 (app (λ(ps) (def p (first ps)) (list (pict-width p) (pict-height p)))
                      (list w h))))
    xs)
  (curve-pict-width w) 
  (curve-pict-height h))


(define (trans-logical-to-device win device-width device-height)
  ; minx, maxx, miny, and, maxy defines a logical coordinate system
  ; device-width and device-height is the device size
  (defm (window minx maxx miny maxy) win)
  (def lw (- maxx minx)) ; logical width
  (def lh (- maxy miny)) ; logical height
  (defv (dw dh) (values device-width device-height))
  ; transform xmin..xmax , ymin..ymax to standard device coords
  ;   xnew = xx*x + xy*y + x0
  ;   ynew = yx*x + yy*y + y0
  (def xx    (/ dw lw))               ; scale from logical x to device x
  (def yy (- (/ dh lh)))              ; scale from logical y to device y
  (def x0 (- (* (/ dw lw) minx)))     ; additional amount added to the device x
  (def y0    (* (/ dh lh) maxy))      ; additional amount added to the device y
  (trans xx 0 0 yy x0 y0))

(define stdtrans trans-logical-to-device)

(define (current-curve-transformation)
  (stdtrans (curve-pict-window) (curve-pict-width) (curve-pict-height)))

(define (px a [T #f])
  (let ([T (or T (current-curve-transformation))])
    (sqrt (+ (sqr (xpx a T)) (sqr (ypx a T))))))

(define (xpx w [T #f])  ; logical width of w pixels 
  (let ([T (or T (current-curve-transformation))])
    (/ w (norm (vec- (T (vec 1 0))   
                     (T (vec 0 0)))))))

(define (ypx h [T #f])
  (let ([T (or T (current-curve-transformation))])
    (/ h (norm (vec- (T (vec 1 0))   
                     (T (vec 0 0)))))))

; (devpt expr ...]
;   receive x and y device coordinate from the last expr,
;   return pt in logical coordinates
(define-syntax (devpt stx)
  (syntax-parse stx
    [(_ expr ...)
     #'(let ()
         (defv (x y) expr ...)
         (def Tinv (inverse (current-curve-transformation)))
         (Tinv (pt x y)))]))

