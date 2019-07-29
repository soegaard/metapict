#lang racket
(require "../main.rkt" "../parameters.rkt")
    
(ahangle         45)       ; default head angle 45 degrees
(ahflankangle    0)        ; default "curvature" of flank (in degrees)
(ahtailcurvature 0)        ; default "curvature" of the back  todo!
(ahratio         1)

    
; (define n1 (circle-node (pt 0 0) .1))
; (define n2 (circle-node (pt 1 0) .1))
; (define n3 (square-node (pt 0 1) .1))
; (define n4 (circle-node (pt 1 1) .1))
    
#;(margin 5
          (scale 4 (draw (draw-node n1)
                         (draw-node n2)
                         (draw-node n3)
                         (filled-node n4)
                         (draw-edge n1 n2)
                         (draw-edge n1 n3 west west)
                         (draw-edge n1 n4))))

;;

;; Hagen's example (from TikZ manual)

(curve-pict-window (window -3 3 -3 3))
(set-curve-pict-size  400 400)
(current-node-size 0.1)
(def waiting        (circle-node "waiting"   #:at (pt  0  1)))
(def critical       (circle-node "critical"  #:at (pt  0  0)))
(def semaphore      (circle-node "semaphore" #:at (pt  0 -1)))
(def leave-critical (square-node #:at (pt  1  0)))
(def enter-critical (square-node #:at (pt -1  0)))
(def capacity       (text-node "s≤3" #:below semaphore))

;        waiting
; enter  critical  leave
;        semaphore


(margin 5
        (scale 2
               (draw (color "gray" (grid (pt -2 -2) (pt 2 2)))
                     waiting
                     critical
                     semaphore
                     leave-critical
                     enter-critical
                     capacity
                     (edge enter-critical critical #:arrow '-> #:label "5")
                     (edge waiting enter-critical left up #:label "4")
                     (edge enter-critical semaphore down left #:label "3")
                     ; --
                     (edge critical leave-critical)
                     (edge semaphore leave-critical right down #:label "1")
                     (edge leave-critical waiting up right #:label "2"))))

(curve-pict-window (window -3 3 -3 3))
(set-curve-pict-size  1200 1200)
(current-node-size 0.3)
(current-label-gap 0.15)

;(def B (circle-node "" #:at (pt  0  2)))
;(def A (circle-node "" #:at (pt -1  1)))
;(def C (circle-node "" #:at (pt  1  1)))
;(def D (circle-node "" #:at (pt  0  0)))
;(def E (circle-node "" #:at (pt  0 -1)))


#;(margin 5
        (scale 1 (draw ; (grid (pt -3 -3) (pt 3 3))
                  A B C D E
                  (edge A B #:label "0,1,L")
                  (edge A C #:label "1,1,R")
                  ;(edge B B) ; todo
                  (edge B C #:label "0,1,L")
                  (edge C D #:label "0,1,L")
                  (edge C E down right #:label "1,0,R")
                  (edge D A #:label "0,1,R")
                  ;(edge D D) ; todo
                  (edge E A left down #:label "1,0,R"))))


; ----

#;(let ()
  (curve-pict-window (window -3 3 -3 3))
  (set-curve-pict-size  800 800)
  (current-node-size 0.1)
  (current-label-gap 0.15)

  (def A (rectangle-node #:at (pt  0  0) #:width 0.5))
  (def B (circle-node "" #:at (pt  1  0)))
  (def C (circle-node "" #:at (pt  1  1)))
  (def D (circle-node "" #:at (pt  0  1)))
  (def E (circle-node "" #:at (pt  -1 1)))
  (def F (circle-node "" #:at (pt  -1 0)))
  (def G (circle-node "" #:at (pt  -1 -1)))
  (def H (circle-node "" #:at (pt  0 -1)))
  (def I (circle-node "" #:at (pt  1 -1)))
  (def J (text-node   "foo" #:at (pt 0 -2) #:direction down))


  (margin 5
          (scale 1 (draw (color "gray" (grid (pt -3 -3) (pt 3 3)))
                    A B C D E F G H I J
                    (edge A B)
                    (edge A C)

                    (edge A D)
                    (edge A E)
                    (edge A F)
                    (edge A G)
                    (edge A H)
                    (edge A I)))))
