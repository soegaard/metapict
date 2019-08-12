#lang racket
(require metapict 
         (prefix-in pict: pict))

; Metapict reexports most functions from pict,
; but there are a few names clashes.
; For example `rectangle` which normally produces a pict,
; but in MetaPict produces a curve.
; Since we need rectangle in the example, they are imported
; with the prefix  pict:

; First we produce a pict to draw on top of.
(define p1 (disk 60))
(define p2 (pict:rectangle 60 60))
(define p3 (hc-append p1 p2))
(define p4 (hc-append p3 (pict:arrow 60 0)))

; The size of the pict is:
(def w   (pict-width p4))
(def h   (pict-height p4))
(def h/2 (/ h 2))

; We now set the logical coordinate system to match.
(set-curve-pict-size w h)
(curve-pict-window (window 0 w 0 h))
(ahlength (px 4))

; This is almost enough, but MetaPict uses a
; normal y-axis, while the pict y-axis is flipped.
; To use standard pict coordinates we need to flip the
; y-axis around y=h/2.

; T : pict coordinates -> metapict coordinates
(def T (shifted 0 h/2 (flipy (shifted 0 (- h/2)))))

(define (find-pt finder base dest)
  (let-values ([(x y) (finder base dest)])
    (T (pt x y))))

(draw p4
      (draw-arrow (curve (find-pt lb-find p4 p2)
                         ..
                         (find-pt cc-find p4 p2)
                         ..
                         (find-pt rc-find p4 p2))))

                    
