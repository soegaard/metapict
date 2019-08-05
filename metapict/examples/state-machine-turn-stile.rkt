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
  (define ms (px 45))                   ; minimum size of nodes

  ; We have three nodes in the example:
  (def L (circle-node "Locked"                               #:min-size ms     #:shade 'axis))
  (def U (circle-node "Unlocked" #:right-of L                #:min-size ms     #:shade 'axis))
  (def S (circle-node            #:below    L #:dist (px 20) #:min-size (px 8) #:fill "black"))


  ; draw title, nodes and edges
  (draw (font-size 24 (text-node "Turnstile" #:at (pt 4 8)))
        L U S               
        (edge L U ne    #:label "coin"  #:label-time 0.1)
        (edge U L sw    #:label "push"  #:label-time 0.1)
        (edge U U right #:label "coin"  #:label-time 0.8)
        (edge L L left  #:label "push"  #:label-time 0.8)
        (edge S L)))


(with-window (window -15 35 -25 25)
  (current-label-gap (px 10))           ; gap between edge and label
  (ahlength (px 6))                     ; size of the arrow head 
  (define ms (px 30))                   ; minimum size of nodes
  (current-ring-gap (px 6))

  (def q1  (circle-node "q1"               #:min-size ms #:shade 'axis))
  (def q2  (circle-node "q2" #:right-of q1 #:min-size ms #:shade 'axis #:rings 1))
  (def q3  (circle-node "q3" #:right-of q2 #:min-size ms #:shade 'axis))

  (def q1- (circle-node #:left-of q1 #:dist (px 20))) ; <---

  (scale 2
  (draw q1 q2 q3
        (edge q1 q2    #:label "1")
        (edge q2 q3 ne #:label "0")
        (edge q3 q2 sw #:label "0, 1")
        (edge q1 q1 up #:label "0")
        (edge q1- q1)
        )))

