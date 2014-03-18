#lang racket/base
(require "structs.rkt" racket/match)

(provide circle-node          ; create node shaped as a circle
         square-node          ; create node shaped as a square
         draw-node            ; draw node
         draw-edge            ; draw edge from one to node to another
         anchor               ; find anchor i.e. point on the outline
         normal               ; find normal vector to the outline 
         current-node-size)   ; default size for circles and half-diameter for squares

; A NODE has 
;  - a position pos   the node is centered over pos
;  - a curve          the curve determines the outline of the node
;  - anchor           vec -> pt function, returns a point on the outline in the given direction
;  - normal           vector normal to the outline pointing outwards
; (struct node (pos curve anchor normal) #:transparent)

(define (anchor n v) ((node-anchor n) v))
(define (normal n v) ((node-normal n) v))

;; Defaults
; current-node-side determines the radius in circles and
; the half-diameter for squares when no size is given.
(define current-node-size (make-parameter 0.2)) ; which unit?


; TODO: Support nodes in draw and fill in draw.rkt
; TODO: Improve anchors and normals for square nodes.

#;(with-window (window -3 3 -3 3)
    (def n (circle-node (pt 0 0)))
    (def m (square-node (pt 1 1)))
    (margin 5
          (scale (draw (draw-node n)
                       (filled-node m)
                       (draw-edge n m)
                       (label-bot "1" (anchor n down)))
                 4)))

(define (circle-node . args)
  (match args
    [(list (? number? x) (? number? y))  (circle-node x y (current-node-size))]
    [(list x y r)                        (circle-node (pt x y) r)]
    [(list (? pt? p))                    (circle-node p (current-node-size))]
    [(list p r)   (define (anchor v) (pt+ p (vec* (/ r (norm v)) v)))
                  (define (normal v) (vec* (/ 1 (norm v)) v))
                  (node p (circle p r) anchor normal)]
    [_            (error 'circle-node "expected a position and a radius")]))

(define (square p r)
  (def -r (- r))
  (shifted p
           (curve (pt -r -r) --
                  (pt  r -r) --
                  (pt  r  r) --
                  (pt -r  r) -- cycle)))

(define (square-node . args)
  (match args
    [(list (? number? x) (? number? y))  (square-node x y (current-node-size))]
    [(list x y r)                        (square-node (pt x y) r)]
    [(list (? pt? p))                    (square-node p (current-node-size))]
    [(list p r)   (define (normal v) 
                    (def α (angle v))
                    (cond [(<=    0 α  π/4) right]
                          [(<=  π/4 α 3π/4) up]
                          [(<= 3π/4 α 5π/4) left]
                          [(<= 5π/4 α 7π/4) down]
                          [else             right]))
                  (define (anchor v) (pt+ p (vec* r (normal v))))
                  (node p (square p r) anchor normal)]
    [_            (error square-node "expected a position and a side length")]))


(define (draw-node n)
  (draw (node-curve n)))

(define (filled-node n)
  (filldraw (node-curve n)))

(define (draw-edge n1 n2 . args)
  (def p1 (node-pos n1))
  (def p2 (node-pos n2))
  (def v  (pt- p2 p1))
  (match args
    [(list)    (draw-edge n1 n2 v (vec* -1 v))]
    [(list v1) (draw-edge n1 n2 v1 (vec* -1 v))]
    [(list v1 v2)
     (draw-arrow
      (cond 
        [(vec= v1 (vec* -1 v2))        ; the special case 
         (def m (pt+ p1 (vec* 0.5 v))) ; "mid" point to achieve a prettier path
         (curve (anchor n1 v1) (normal n1 v1) .. m .. (vec* -1 (normal n2 v2)) (anchor n2 v2))]
        [else                          ; the general case
         (curve (anchor n1 v1) (normal n1 v1) .. (vec* -1 (normal n2 v2)) (anchor n2 v2))]))]))


;;; TESTING

(require metapict)
  
(define n1 (circle-node (pt 0 0) .1))
(define n2 (circle-node (pt 1 0) .1))
(define n3 (square-node (pt 0 1) .1))
(define n4 (circle-node (pt 1 1) .1))

(margin 5
          (scale (draw (draw-node n1)
                       (draw-node n2)
                       (draw-node n3)
                       (filled-node n4)
                       (draw-edge n1 n2)
                       (draw-edge n1 n3 west west)
                       (draw-edge n1 n4))
                 4))
