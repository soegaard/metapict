#lang racket
(require "../main.rkt" "../parameters.rkt" )

(ahangle         45)       ; default head angle 45 degrees
(ahflankangle    0)        ; default "curvature" of flank (in degrees)
(ahtailcurvature 0)        ; default "curvature" of the back  todo!
(ahratio         1)

(set-curve-pict-size  400 400)
(current-node-size 0.1)


(def O origo)
(def P (pt 10 5))
(def OP (pt- P O))
(def PO (vec* -1 OP))

(def T (shifted P (yscaled 0.5 (shifted (pt- P)))))
(displayln T)


(def C (circle P 5))

(with-window (window -30 30 -30 30)
  (draw (grid (pt -30 -30) (pt 30 30) #:step 5)
        C
        (T C)))

(current-test-value 0.2)

(with-window (window -30 30 -30 30)
  (draw (grid (pt -30 -30) (pt 30 30) #:step 5)
        (rounded-rectangle-node #:at P
                                #:shade 'axis
                                #:fill "green"
                                #:min-size 20)))




