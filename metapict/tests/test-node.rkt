#lang racket
(require "../main.rkt" "../parameters.rkt")
    
(ahangle         45)       ; default head angle 45 degrees
(ahflankangle    0)        ; default "curvature" of flank (in degrees)
(ahtailcurvature 0)        ; default "curvature" of the back  todo!
(ahratio         1)

(set-curve-pict-size  400 400)
(current-node-size 0.1)

(let ()
  ; test placement relative to the node A
  (curve-pict-window (window -3 3 -3 3))
  (def A (circle-node "A"))
  (def B (circle-node "B" #:right-of A))
  (def C (circle-node "C" #:above    A))
  (def D (circle-node "D" #:left-of  A))
  (def E (circle-node "E" #:below    A))
  (draw A B C D E
        (edge A B)
        (edge A C)
        (edge A D)
        (edge A E)))

(let ()
  ; test different arrow types
  (curve-pict-window (window -3 3 -3 3))
  (def A (circle-node "A"))
  (def B (circle-node "B" #:right-of A))
  (def C (circle-node "C" #:above    A))
  (def D (circle-node "D" #:left-of  A))
  (def E (circle-node "E" #:below    A))
  (draw A B C D E
        (edge A B #:arrow '-)
        (edge A C #:arrow '<->)
        (edge A D #:arrow '->)
        (edge A E #:arrow '<-)))

(let ()
  ; test automatic placement of labels
  (curve-pict-window (window -3 3 -3 3))
  (def A (circle-node "A"))
  (def B (circle-node "B" #:right-of A))
  (def C (circle-node "C" #:above    A))
  (def D (circle-node "D" #:left-of  A))
  (def E (circle-node "E" #:below    A))
  (draw A B C D E
        (edge A B #:label "AB")
        (edge A C #:label "AC")
        (edge A D #:label "AD")
        (edge A E #:label "AE")))

(let ()
  ; test leaving and entering directions of an edge
  (curve-pict-window (window -3 3 -3 3))
  (def A (circle-node "A"))
  (def B (circle-node "B" #:right-of A))
  (def C (circle-node "C" #:above    A))
  (def D (circle-node "D" #:left-of  A))
  (def E (circle-node "E" #:below    A))
  (draw A B C D E
        (edge A B (vec 1  1) down)
        (edge A B (vec 1 -1) up)
        (edge A C (vec+ up right) left)
        (edge A C (vec+ up left)  right)
        (edge A D (dir 135)  (dir -90))  ; angle in degrees
        (edge A D (dir 225)  (dir 90))
        (edge A E (dir -45)  (dir 180))
        (edge A E (dir -135) right)))


(let ()
  ; test leaving and entering directions of an edge
  ; where only one is given
  (curve-pict-window (window -3 3 -3 3))
  (def A (circle-node "A"))
  (def B (circle-node "B" #:right-of A))
  (def C (circle-node "C" #:above    A))
  (def D (circle-node "D" #:left-of  A))
  (def E (circle-node "E" #:below    A))
  (draw A B C D E
        ; specify which direction to leave A
        (edge A B down  #f)
        (edge A B up    #f)
        (edge A C right #f)
        (edge A C left  #f)
        ; specify direct to enter the from node
        (edge A D #f down)  
        (edge A D #f up)
        (edge A E #f left)
        (edge A E #f right)))

(let ()
  ; test placments with   #:at <coordinate>
  ; todo: fix circle-node with "" as input text
  ;     (grid lower-left-corner upper-right-corner)
  ; helps finding the coordinates 
  (def n1 (circle-node " " #:at (pt 0 0)))
  (def n2 (circle-node " " #:at (pt 1 0)))
  (def n3 (square-node " " #:at (pt 0 1)))
  (def n4 (circle-node " " #:at (pt 1 1)))
  
  (draw (color "gray" (grid (pt -2 -2) (pt 2 2)))
        n1 n2 n3
        (filled-node n4)
        (edge n1 n2)
        (edge n1 n3 west)
        (edge n1 n4)))

;;

;; Hagen's example (from TikZ manual)

(let ()
  ; todo: square-node needs to support #:right-of and friends
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
  
  
  (scale 1.5 
         (draw ; (color "gray" (grid (pt -2 -2) (pt 2 2)))
          ; the nodes
          waiting critical semaphore leave-critical  enter-critical capacity
          ; the edges
          (edge enter-critical critical             #:label "5")
          (edge waiting enter-critical   left down  #:label "4")
          (edge enter-critical semaphore down right #:label "3")
          ; --
          (edge critical leave-critical)
          (edge semaphore leave-critical right up   #:label "1")
          (edge leave-critical waiting   up    left #:label "2"))))

(let ()
  ; a little state machine
  (curve-pict-window (window -3 3 -3 3))
  (current-node-size 0.3)
  (current-label-gap 0.15)


  (def B (circle-node " " #:at (pt  0  2)))  ; top
  (def A (circle-node " " #:at (pt -1  1)))  ; middle
  (def C (circle-node " " #:at (pt  1  1)))  ; middle
  (def D (circle-node " " #:at (pt  0  0)))  ;   
  (def E (circle-node " " #:at (pt  0 -1)))  ; lowest


  (margin 5
          (scale 1  (draw A B C D E
                          (edge A B #:label "01L")
                          (edge A C #:label "11R")
                          (edge B C #:label "01L")
                          (edge C D #:label "01L")
                          (edge C E down left #:label "10R")
                          (edge D A #:label "01R")
                          
                          (edge E A left up #:label "10R")
                          
                          ; loop from B to B
                          (edge B B (dir 60) (dir -60)
                                #:via (pt 0 2.7)) ; todo: improve loops
                          ; loop from D to D
                          (edge D D (dir -60) (dir 60) #:via (pt 0 -0.7))
                          ))))


; ----

(let ()
  ; test arrow placement for a rectangular node
  (curve-pict-window (window -3 3 -3 3))
  ; (set-curve-pict-size  800 800)
  (current-node-size 0.1)
  (current-label-gap 0.15)

  (def A (rectangle-node #:at (pt  0  0) #:width 0.5))
  (def B (circle-node " " #:at (pt  1  0)))
  (def C (circle-node " " #:at (pt  1  1)))
  (def D (circle-node " " #:at (pt  0  1)))
  (def E (circle-node " " #:at (pt  -1 1)))
  (def F (circle-node " " #:at (pt  -1 0)))
  (def G (circle-node " " #:at (pt  -1 -1)))
  (def H (circle-node " " #:at (pt  0 -1)))
  (def I (circle-node " " #:at (pt  1 -1)))
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
