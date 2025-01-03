#lang racket
(require metapict)
;;; EXAMPLE
;;;   Inspiration:
;;;     http://www.texample.net/tikz/examples/rooty-helix/

(def max-r 86)

(def dark-green   (make-color* 175 193 36))
(def almost-black (make-color*  50  50 50))

(define (shade r) ; white -> dark-green -> almost-black
  (cond
    [(<= 0 r 1/2) (color-med (* 2 r)         "white"    dark-green   )]
    [(<=   r 1)   (color-med (* 2 (- r 1/2)) dark-green almost-black )]
    [else         (error 'shader (~a "got: " r))]))

(define (spiral drawing max-r)
  (def (node p r)
    (def circ     (circle p 1.5))
    (def filled   (color "white" (fill circ)))
    (def label    (label-cnt (~a r) p))
    (draw filled circ label))
  (defv (spiral θ)
    (for/fold ([drawing drawing] [θ 0])  
              ([r (in-range 1 max-r)])
      (def √r (sqrt r))
      (def (rotθ c) (scaled 4 (rotated θ c)))
      (defv (A B C) (values (pt 0 0) (rotθ (pt √r 0)) (rotθ (pt √r 1))))
      (def triangle (curve A -- B -- C -- cycle))
      (def filled   (color (shade (/ r 86)) (fill triangle)))
      (values (draw drawing filled triangle (node B r))
              (+ θ (acos (sqrt (/ r (+ 1 r))))))))
  (draw spiral 
        (node (scaled 4 (pt@ (sqrt max-r) θ)) max-r)))

(set-curve-pict-size 600 600)
(with-window (window -40 40 -40 40)
  (penwidth 0
    (for/fold ([drawing (draw)]) ([r '(86 38 15)])
      (spiral drawing r))))


