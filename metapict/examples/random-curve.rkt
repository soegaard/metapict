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

(define (random-star r1 r2 tips noise)
  ; r1   is the inner radius
  ; r2   is the outer radius
  ; tips is the number of tips
  ; noise is the noise level σ
  (def N (normal-dist 0 noise))
  (define (random-radius r) (+ r (sample N)))
  (def step (/ 360 tips))
  (def pts (append*
            (for/list ([d (in-range 0 360 step)])
              (list (pt@d (random-radius r1) d)
                    (pt@d (random-radius r2) (+ d (/ step 2)))))))
  (def c (curve* (add-between (append pts (list cycle)) --)))
  c)

"Series: increasing number of tips, constant noise"
(with-window (scale-window 4 (window -1 (+ -1 (* t 2))  -1 1))
  (for/draw ([n (in-range 0 0.7 (/ 0.7 t))]
             [i t])
    (shifted (* i 6) 0
             (random-star 2 3 (+ 4 i) 0.2))))

"Series: fixed number of tips, increasing noise"
(with-window (scale-window 4 (window -1 (+ -1 (* t 2))  -1 1))
  (for/draw ([n (in-range 0 0.7 (/ 0.7 t))]
             [i t])
    (shifted (* i 6) 0
             (random-star 1.5 2.5 12 (* 0.04 i)))))




  

