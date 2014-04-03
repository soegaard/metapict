#lang racket
(require metapict metapict/graph)

(define (wave len amplitude)
  (define (f x)  (* amplitude (sin (/ (* x 2pi) len))))
  (define (df x) (* (/ amplitude 2pi len) (cos (/ (* x 2pi) len))))
  (graph f 0 len #:samples 16 #:diff df))

(define (snake p q #:length [len 1] #:amplitude [amplitude (/ 1 pi)])
  (def pq (pt- q p))
  (def l (norm pq))
  (def x (/ l len))
  (def n (floor x))
  (def w (wave len amplitude))
  (def wavy-part (for/fold ([c w]) ([i (in-range 1 n)])
                   (curve-append c (shifted (* i len) 0 w))))
  (def end (end-point wavy-part))
  (def s (if (< (dist end (pt l 0)) 1e-10)
             wavy-part
             (curve-append wavy-part (curve end -- (pt l 0)))))
  (shifted p (rotated (angle2 east pq) s)))

(set-curve-pict-size 200 200)
(with-window (window -5 5 -5 5)
  (draw (color "gray" (draw (grid (pt -10 -10) (pt 10 10))))
        (snake (pt -2 0) (pt 2 0) #:length 1/2)))


