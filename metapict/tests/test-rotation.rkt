#lang racket
(require metapict)

; bitmap size
(set-curve-pict-size  800 800)

; logical coordinatesystem  xmin=-2, xmax=2, ymin=-2, ymax=2
(curve-pict-window (window -2 2 -2 2))

(for/draw ([i 100])
  (color (color-med (* 0.01 i) "red" "blue")
         (fill
          (rotated (* 0.54 i)
                   (scaled (* 0.01 (- 100 i))
                           unitsquare)))))

(set-curve-pict-size  600 600)
(curve-pict-window (window -2 2 -2 2))
(ahlength 0.08)

(def P  (text-node "P"))
(def B  (text-node "B" #:right-of P))
(def A  (text-node "A" #:below    P))
(def C  (text-node "C" #:below    B))
(def P^ (text-node "P^" #:at (pt -1 1)))
(draw ; (color "gray" (grid (pt -2 -2) (pt 2 2)))
       P B A C P^ 
      (edge P B  #:label "f bar")
      (edge P A  #:label "g bar")
      (edge A C  #:label "f")
      (edge B C  #:label "g")
      (edge P^ A #:label "g hat" down left)
      (edge P^ B #:label "f hat" right up)
      (edge P^ P #:label "k"))

