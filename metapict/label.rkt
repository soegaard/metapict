#lang racket/base
;;; Labels
; A label consists of a picture and a position in logical coordinates.
;    (struct label (string-or-pict pos plc) #:transparent)
; Labels are affected by the following parameters:
;   current-font-size
;   current-label-size
;   label-offset

; label-bbox ; curve given by the bounding box of the label
; fill-label ; use the color c to fill the box of the label, then draw l

(provide (all-defined-out)) ; todo

(require (for-syntax racket/base) racket/match racket/format
         "pict-lite.rkt"
         "def.rkt" "device.rkt" "draw.rkt"  "pt-vec.rkt" "structs.rkt" 
         "pict.rkt" "parameters.rkt" "curve.rkt" "path.rkt" "color.rkt")

(require (for-syntax syntax/parse))
(define-syntax (define-label-plc stx)
  (syntax-parse stx
    [(_ name plc)     
     (syntax/loc stx
       (define (name text/pict pos)
         (label text/pict pos (plc))))]))

(define (dot-label pict pos [plc (bot)])
  (penscale 4 (draw pos (label pict pos plc))))

(define-label-plc label-top  top)
(define-label-plc label-bot  bot)
(define-label-plc label-lft  lft)
(define-label-plc label-rt   rt)
(define-label-plc label-ulft ulft)
(define-label-plc label-urt  urt)
(define-label-plc label-llft llft)
(define-label-plc label-lrt  lrt)
(define-label-plc label-cnt  cnt)

(define-syntax (define-dot-label-plc stx)
  (syntax-parse stx
    [(_ name plc)
     (syntax/loc stx
       (define (name text/pict pos)
         (dot-label text/pict pos (plc))))]))

(define-dot-label-plc dot-label-top  top)
(define-dot-label-plc dot-label-bot  bot)
(define-dot-label-plc dot-label-lft  lft)
(define-dot-label-plc dot-label-rt   rt)
(define-dot-label-plc dot-label-ulft ulft)
(define-dot-label-plc dot-label-urt  urt)
(define-dot-label-plc dot-label-llft llft)
(define-dot-label-plc dot-label-lrt  lrt)
(define-dot-label-plc dot-label-cnt  cnt)

(define (label-bbox l)
  (defm (label p? pos placement) l)
  (defm (pt x0 y0) pos)
  (def p (if (pict? p?) p? (text p? null (current-font-size))))
  (defv (w h) (values (pict-width p) (pict-height p)))
  (defv (-w -h) (values (- w) (- h)))
  (defv (-w/2 -h/2) (values (/ -w 2) (/ -h 2)))
  (def ε (label-offset)) ; in pixels
  (def -ε (- ε))
  (defm (vec Δx Δy) ; upper left corner
    (match placement
      [(rt)   (vec+ (vec 0    -h/2) (vec  ε  0))]
      [(lft)  (vec+ (vec -w   -h/2) (vec -ε  0))]
      [(bot)  (vec+ (vec -w/2  0)   (vec  0  ε))]
      [(top)  (vec+ (vec -w/2 -h)   (vec  0 -ε))]
      [(cnt)  (vec+ (vec -w/2 -h/2) (vec  0  0))]            
      [(lrt)  (vec+ (vec  0    0)   (vec  ε  ε))]   ; lft is +
      [(ulft) (vec+ (vec -w   -h)   (vec -ε -ε))]            
      [(llft) (vec+ (vec -w    0)   (vec -ε  ε))]
      [(urt)  (vec+ (vec  0   -h)   (vec  ε -ε))]  ; *
      [else   (error 'label->pict (~a "internal error, expected a placement:" placement))]))
  (def T (current-curve-transformation))
  ; convert from screen coordinates to logical coordinates
  (let ([Δx (xpx Δx T)] [Δy (- (ypx Δy T))] [w (xpx w T)] [h (ypx h T)])
    (def -h (- h))
    (curve    (pt (+ x0 Δx)   (+ y0 Δy))
              -- (pt (+ x0 Δx w) (+ y0 Δy))
              -- (pt (+ x0 Δx w) (+ y0 Δy -h))
              -- (pt (+ x0 Δx)   (+ y0 Δy -h)) -- cycle)))

(define (fill-label fill-color l)
  (draw (color fill-color (fill (label-bbox l)))
        l))
