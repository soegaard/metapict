#lang racket
(require metapict)

;;; EXAMPLE
;;;   Inspiration:
;;;     http://www.texample.net/tikz/examples/rooty-helix/

; TODO: Fix overlap at nodes 38 and 15.

(def max-r 86)

(def dark-green   (make-color* 175 193 36))
(def almost-black (make-color*  50  50 50))

(define (shade r)
  (cond
    [(<= 0 r 1/2) (color-med (* 2 r)         dark-green   "white")]
    [(<=   r 1)   (color-med (* 2 (- r 1/2)) almost-black dark-green)]
    [else         (error 'shader (~a "got: " r))]))

(current-font-size 2)

(define (spiral drawing max-r)
  (defv (spiral _)
    (for/fold ([drawing drawing] [θ 0])  ([r (in-range 1 max-r)])
      (def √r (sqrt r))
      (def (rotθ c) (scaled 4 (rotated θ c)))
      (defv (A B C) (values (pt 0 0) (rotθ (pt √r 0)) (rotθ (pt √r 1))))
      (def triangle (curve A -- B -- C -- cycle))
      (def filled   (color (shade (/ r 86)) (fill triangle)))
      (def circle   (circle-curve (pt-x B) (pt-y B) 1.5))
      (def node     (color "white" (fill circle)))
      (def lab      (label-cnt (~a r) B))
      (values (penwidth 0 (draw drawing filled triangle node circle lab))
              (+ θ (acos (sqrt (/ r (+ 1 r))))))))
  spiral)

(with-window (window -40 40 -40 40)
  (scale (for/fold ([drawing (draw)]) ([r '(86 38 15)])
           (spiral drawing r))
         8))


