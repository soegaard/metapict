#lang racket
(require "../metapict.rkt")

; convenient short hand
(define ne north-east)
(define nw north-west)
(define se south-east)
(define sw south-west)

; size of the output bitmap
(set-curve-pict-size 400 400)

; setup the logical coordinates coordinate system

(with-window (window -15 35 -25 25)
  (current-label-gap (px 10))           ; gap between edge and label
  (current-neighbour-distance (px 50))  ; gap between neighbour nodes
  (ahlength (px 6))                     ; size of the arrow head
  ; The size of nodes depends on the size of the contents (text or pict),
  ; it is prettier with the same size for all nodes, so we set it here.
  (current-node-minimum-size (px 45))

  ; We have three nodes in the example (Locked, Unlocked and Start):
  (def L (circle-node "Locked"                 #:shade 'axis))
  (def U (circle-node "Unlocked" #:right-of L  #:shade 'axis))
  (def S (circle-node            #:below    L
                                 #:dist (px 20) #:min-size (px 8) #:fill "black"))

  ; draw title, nodes and edges
  (draw (font-size 24 (text-node "Turnstile" #:at (pt 4 8) #:shade 'none))
        L U S               
        (edge L U ne    #:label "coin"  #:label-time 0.1) 
        (edge U L sw    #:label "push"  #:label-time 0.1)
        (edge U U right #:label "coin"  #:label-time 0.8) 
        (edge L L left  #:label "push"  #:label-time 0.8)
        (edge S L)))


(with-window (window -15 35 -25 25)
  (current-label-gap (px 10))           ; gap between edge and label
  (ahlength (px 6))                     ; size of the arrow head 
  (current-node-minimum-size (px 45))    ; minimum size of nodes
  (current-ring-gap (px 6))
  (current-node-shade 'axis)
  (current-neighbour-distance (px 40))
  (current-incoming-edge-size (px 20))

  ; There are three states. The accept state q2 gets an extra ring around the node.
  (def q1  (circle-node "q1"))
  (def q2  (circle-node "q2" #:right-of q1  #:rings 1)) 
  (def q3  (circle-node "q3" #:right-of q2))

  (scale 1
  (draw q1 q2 q3
        (edge q1 q2    #:label "1")
        (edge q2 q3 ne #:label "0")
        (edge q3 q2 sw #:label "0, 1")
        (edge q1 q1 up #:label "0")
        (edge #f q1))))


(with-window (window -15 35 -25 25)
  (current-label-gap (px 8))            ; gap between edge and label
  (ahlength (px 6))                     ; size of the arrow head 
  (define ms (px 30))                   ; minimum size of nodes
  (current-ring-gap (px 6))
  (current-neighbour-distance (px 75))
  (current-incoming-edge-size (px 20))
  (current-outgoing-edge-size (px 20))
  
  (def n2  (circle-node "2"                  #:min-size ms #:shade 'axis))
  (def n3  (circle-node "3" #:right-of n2    #:min-size ms #:shade 'axis))
  (def n1  (circle-node "1" #:at (pt 9.3 16) #:min-size ms #:shade 'axis #:rings 1))

  (scale 1
  (draw n1 n2 n3
        (edge n1 n2                      #:label "b")
        (edge n2 n3                      #:label "a, b" #:label-dir down)
        (edge n1 n3 (dir -30) (dir 280)  #:label "ε") ; todo
        (edge n3 n1 (dir 135) (dir 100)  #:label "a")
        (edge n2 n2 left                 #:label "a")
        (edge #f n1)))) ; incoming edge into n1


(with-window (window -15 35 -25 25)
  (current-label-gap (px 8))            ; gap between edge and label
  (ahlength (px 6))                     ; size of the arrow head 
  (define ms (px 30))                   ; minimum size of nodes
  (current-ring-gap (px 6))
  (current-neighbour-distance (px 75))
  (current-incoming-edge-size (px 20))
  (current-outgoing-edge-size (px 20))
  
  (def n2  (circle-node "2"                  #:min-size ms #:shade 'axis))
  (def n3  (circle-node "3" #:right-of n2    #:min-size ms #:shade 'axis))
  (def n1  (circle-node "1" #:at (pt 9.3 16) #:min-size ms #:shade 'axis #:rings 1))

  (scale 1
  (draw n1 n2 n3
        (edge n1 n2                      #:label "b")
        (edge n2 n3                      #:label "a, b" #:label-dir down)
        (edge n1 n3 (dir -30) (dir 280)  #:label "ε") ; todo
        (edge n3 n1 (dir 135) (dir 100)  #:label "a")
        (edge n2 n2 left                 #:label "a")
        (edge #f n1)))) ; incoming edge into n1

(set-curve-pict-size 1200 1200)
(with-window (window -100 400 -250 250)
  (current-label-gap (px 8))            ; gap between edge and label
  (ahlength (px 6))                     ; size of the arrow head 
  (define ms (px 30))                   ; minimum size of nodes
  (current-ring-gap (px 6))
  (current-neighbour-distance (px 50))
  (current-incoming-edge-size (px 15))
  (current-outgoing-edge-size (px 20))
  (current-node-minimum-size  (px 30))
  (current-inner-separation   (px 5))
  
  ; top row
  (def t1 (circle-node "∅"))
  (def t2 (circle-node "{1}"     #:right-of t1 #:rings 1))
  (def t3 (circle-node "{2}"     #:right-of t2))
  (def t4 (circle-node "{1,2}"   #:right-of t3 #:rings 1))
  ; bottom row
  (def b1 (circle-node "{3}"     #:below t1))
  (def b2 (circle-node "{1,3}"   #:below t2 #:rings 1))
  (def b3 (circle-node "{2,3}"   #:below t3))
  (def b4 (circle-node "{1,2,3}" #:below t4 #:rings 1))

  (scale 1
    (draw t1 t2 t3 t4
          b1 b2 b3 b4
          (edge t1 t1 left #:label "a,b")
          (edge t2 t1      #:label "a")
          (edge t2 t3      #:label "b")
          (edge t3 b1      #:label "b" #:label-dir up)
          (edge t3 b3      #:label "a")
          (edge t4 b3      #:label "a,b" #:label-dir nw)
          (edge b1 t1      #:label "b")
          (edge b1 b2      #:label "a")
          (edge b2 t3      #:label "b")
          (edge #f b2 down)            ; incoming edge
          (edge b2 b2      #:label "a")
          (edge b3 b1 sw   #:label "b")
          (edge b3 b4       #:label "a" #:label-time 0.7)
          (edge b4 b3 sw   #:label "b")
          (edge b4 b4 up   #:label "a"))))


(set-curve-pict-size 800 400)
(with-window (window -50 150 -50 50)
  (def d        (px 50)) ; ball diameter
  (def balls    7)
  (for/draw ([i (in-range 0 1 (/ 0.5 balls))]
             [α (in-range 0 360 (/ 360. balls))])     
     (def offset (vec* i (vec (- d) d)))
     (circle-node #:at           (pt@d (* 1.5 d) α)
                  #:min-size     d
                  #:shade        'ball
                  #:fill         "red"
                  #:shade-offset offset)))



