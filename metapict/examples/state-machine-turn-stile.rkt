#lang racket
(require "../metapict.rkt")

; convenient short hand
(define ne north-east)
(define nw north-west)
(define se south-east)
(define sw south-west)

; size of the output bitmap
(set-curve-pict-size 800 800)

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
  (scale 2
         (draw (font-size 24 (text-node "Turnstile" #:at (pt 4 8)))
               L U S               
               (edge L U ne    #:label "coin" #:label-dir up   #:label-time 0.1)
               (edge U L sw    #:label "push" #:label-dir down #:label-time 0.1)
               (edge U U right #:label "coin" #:label-dir down #:label-time 0.8)
               (edge L L left  #:label "push" #:label-dir nw   #:label-time 0.8)
               (edge S L))))

