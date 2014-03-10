#lang racket/base

(require racket/generic racket/match)

; A NODE has 
;  - a position pos   (the node is centered over pos)
;  - a curve          (the curve determines the outline of the node)
;  - anchor           (vec -> pt function, returns a point on the outline in the given direction)
(struct node (pos curve anchor) #:transparent)

(define (circle-node . args)
  (match args
    [(list x y r) (node (pt x y) r)]
    [(list p r)   (define (anchor v) (vec* (/ (norm v)) v))
                  (node p (circle p r) anchor)]
    [_            (error 'circle-node "expected a position and a radius")]))

(define (anchor n v)
  ((node-anchor n) v))

(define (draw-node n)
  (draw (node-curve n)))

(require metapict)
  
(define n1 (circle-node (pt 0 0) .1))
(define n2 (circle-node (pt 1 0) .1))
(define n3 (circle-node (pt 0 1) .1))
(define n4 (circle-node (pt 1 1) .1))

(draw (draw-node n1)
      (draw-node n2)
      (draw-node n3)
      (draw-node n4))






