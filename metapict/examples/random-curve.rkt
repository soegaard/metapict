#lang racket
(require metapict math/distributions)

(define (random-blob noise-level)
  (define d (normal-dist 0 noise-level))  ; ~ N(0, noise-level)
  (define (random-radius) (+ 1 (sample d)))    
  (def pts (for/list ([d (in-range 0 360 45)]) (pt@d (random-radius) d)))
  (def c (curve* (add-between (append pts (list cycle)) ..)))
  (def l (arc-length c))
  (scaled (/ (* 5 π) l) c))
    

(def t 10)
(set-curve-pict-size (* t 100) 100)
(with-window (scale-window 4 (window -1 (+ -1 (* t 2))  -1 1))
  (for/draw ([n (in-range 0 0.7 (/ 0.7 t))]
             [i t])
            (shifted (* i 6) 0 (random-blob n))))






