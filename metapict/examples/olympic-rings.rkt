#lang racket
(require metapict)

(struct ring (center color))

(define r1 (ring (pt -4  0)   (make-color*   0 129 188)))
(define r2 (ring (pt -2 -1.8) (make-color* 252 177  49)))
(define r3 (ring (pt  0  0)   (make-color*  35  34  35)))
(define r4 (ring (pt  2 -1.8) (make-color*   0 157  87)))
(define r5 (ring (pt  4  0)   (make-color* 238  50  78)))

(define (draw-rings . rings)
  (for/draw ([r rings])
    (defm (ring p c) r)
    (def c1 (circle-curve p 1.9))
    (def c2 (circle-curve p 1.5))
    (def connector (curve (point-of c1 (curve-length c1)) -- (point-of c2 0)))
    (def circle-outline (curve-append c1 connector c2))
    (draw (pencolor c (fill circle-outline))
          (penwidth 4 (color "white" (draw c1 c2))))))

(set-curve-pict-size 600 300)
(with-window (window -6 6 -4 2)
  (draw (clipped (draw-rings r5 r4 r3 r2 r1) (box (pt -6  2)   (pt 6 -0.9)))
        (clipped (draw-rings r1 r2 r3 r4 r5) (box (pt -6 -0.9) (pt 6 -3.8)))))

