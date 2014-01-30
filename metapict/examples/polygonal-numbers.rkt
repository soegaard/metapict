#lang racket
(require metapict)

;;;
;;; EXAMPLE
;;;   Figure from Wikipedia article on polygonal numbers.
;;;

(def (fig-polygonal-numbers)
  (let ()
    (curve-pict-window (window -2.1 2.1 -2.1 2.1))
    (set-curve-pict-size 400 400)
    (def A (pt 0 0))
    (def B (pt 1 0))
    (def Δ (/ (dist A B) 5)) ; space between pt centers (6 dots => 5 spaces)
    (def all-rings
      (append*
       (for/list ([j (in-range 1 6)])
         (def bottom
           (for/list ([i j])
             (shifted (* i Δ) 0 A)))
         (def ring
           (for/fold ([partial-ring '()]) ([i 6])
             (append bottom
                     (map (shifted (* j Δ) 0 (rotatedd 60)) partial-ring))))
         ring)))
    (penwidth 10
              (draw (draw* all-rings)
                    (penscale 1.2 (color "red"  (draw A)))
                    (penscale 1.2 (color "blue" (draw B)))))))

(fig-polygonal-numbers)
