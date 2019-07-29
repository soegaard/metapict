#lang racket/base
(require metapict racket/format racket/match racket/list)

;;; TODO

;; This module was written before nodes were implemented, so
;; it needs to be rewritten to use nodes. 

;;; Tree Drawing

;;; Goals 
; 1. Implement the Reingold-Tilford algorithm for drawing trees.
;        http://emr.cs.iit.edu/~reingold/tidier-drawings.pdf
; 2. Implement the improvement of Buchheim et al
;        http://dirk.jivas.de/papers/buchheim02improving.pdf
; 3. Consider the variations in "Compact layout of Layered Trees"

;;; Reference
; Wetherell and Shannon: Tidy Drawing of Trees
; http://poincare.matf.bg.ac.rs/~tijana/geometrija/seminarski/tree_drawing.pdf
; Bill Mill's blog post "Drawing Presentable Trees"
; http://billmill.org/pymag-trees/

;;; Principles
; 1. Edges should not cross
; 2. All nodes at the same depth should be drawn on a horizontal line. 
;    (And for ordered trees the children should be in the correct order)
; 3. Tree should be drawn as a narrow as possible.


; These functions are for testing purposes.
; The final interface will receive accesors in order
; to make the actual representation of the tree irrelevant.
(define (leaf? t)    (not (pair? t)))
(define (element t)  (car t))
(define (children t) (cdr t))

(module+ test
  ;         A
  ;     B     C
  ;    D E   F G
  ;      H   I
  (define a-tree '(a (b (d) (e (h)))
                     (c (f (i)) (g)))))

; depth : tree -> natural
;   returns the length of the longest path in the tree t
(define (depth t)
  (if (leaf? t)
      1
      (+ 1 (for/fold ([m 0]) ([c (children t)])
             (max m (depth c))))))

; minimum-width-tree-positions : tree ->  hash from trees to pt
;   this naive strategy illustrates computes the placements
;   of all subtrees and returns them as a hash table from
;   subtrees to points (pt). The hash table is to be used
;   as input for draw-tree.
(define (minimum-width-tree-positions t)
  ; Minimum width tree, principle 1, 2, and, 3.  
  ; Track positions of subtrees (not elements).
  (def positions (make-hasheq)) 
  ; nexts[i] = next available slot on row i
  (def nexts     (make-hash))
  (define (recur tree depth)
    (def next (hash-ref! nexts depth 0))
    (hash-set! positions tree (pt next depth))
    (hash-set! nexts depth (+ next 1))
    (for ([c (children tree)])
      (recur c (+ depth 1))))
  (recur t 0)
  positions)

;;; Principles 

; 4. A parent should be centered over its children
(define (simple-centered-tree-positions t)
  ; NOTE: This algorithm assumes t is a binary tree. 
  ;       An extension is needed to support nary trees.
  
  ; Principle 1, 2, and, 4.  (i.e. narrowness not a goal)
  ; Traversal 1: Use post order traversal (root x-pos is mean of childrens)
  ;              Use mods to store horizontal movements of subtrees.
  ; Traversal 2: Add mods to compute final horizontal placement.
  
  ; Track positions of subtrees (not elements).
  (def positions (make-hasheq))
  (define (posn t)    (hash-ref positions t))
  (define (posn! t p) (hash-set! positions t p))
  ; nexts[i] = next available slot on row i
  (def nexts (make-hash))
  (define (next t)    (hash-ref! nexts t 0))
  (define (next! t n) (hash-set! nexts t n))
  
  (def offsets (make-hash))
  (define (offset d)    (hash-ref! offsets d 0))
  (define (offset! d o) #;(displayln (list 'offset! d o)) (hash-set! offsets d o))
  
  ; mod[t]=m means that all nodes in the tree t (except for the root)
  ; should be shifted m to the right in the second traversal.
  (def mods (make-hasheq))
  (define (mod t)    (hash-ref! mods t 0))
  (define (mod! t m) (hash-set! mods t m))
  
  ;; Traversal 1
  (define (recur t depth)
    (for ([c (children t)])
      (recur c (+ depth 1)))
    ; (displayln (list t depth offsets mods))
    (def y depth)
    (def cs (if (leaf? t) '() (children t)))
    (def place (match cs
                 ; a leaf gets the next available place
                 [(list)             (next depth)]
                 ; parents with one child is right above
                 [(list c)           (pt-x (posn c))]
                 ; parents with multiple childs are centered
                 [(list c0 c ... cn) (/ (+ (pt-x (posn c0)) (pt-x (posn cn))) 2)]))
    ; Leafs have been placed in available place, 
    ; but a parent may need to be moved if a the place is taken already.
    ; The offset table holds the total amount of padding needed on a row. 
    ; (i.e. the offset is cumulative)
    (offset! depth (max (offset depth) (+ (- (next depth) place) 1))) ; no op for leafs !
    (def x (if (leaf? t) place (+ place (offset depth))))
    (posn! t (pt x y))
    (next! depth (+ (next depth) 2))
    (mod! t (offset depth)))
  
  ;; Traversal 2
  (define (addmods t modsum)
    ; (displayln (list modsum t))
    (posn! t (pt+ (posn t) (vec modsum 0)))
    (def newmodsum (+ modsum (mod t)))
    (for ([c (children t)])
      (addmods c newmodsum)))
  
  (recur t 0)
  (addmods t 0)
  ; (displayln (list 'positions positions))
  ; (displayln (list 'offsets offsets))
  ; (displayln (list 'mods mods))
  positions)

;;; Principle
; 5. A subtree should be drawn the same no matter where it is in the tree
; TODO: Implement Reingold-Tilford. Use "threads" to describe the contour
; of the tree.

; TODO: Introduce the concept of nodes, and use them to render the
; elements of the tree.
(define (draw-tree tree positions)
  (define (posn t) (hash-ref positions t))
  (define (recur tree drawing)
    (def p (posn tree))
    (def d (draw drawing (color "red" (draw (label-cnt (~a (element tree)) p)))))
    (cond [(leaf? tree) d]
          [else         (draw d (for/draw ([c (children tree)])
                                          (draw (curve p -- (posn c))
                                                (recur c d))))]))
  (recur tree (draw)))


;(ahangle         45)      
;(ahflankangle    0) 
;(ahtailcurvature 0) 
;(ahratio         1)

(module+ test
  (set-curve-pict-size 400 400)
  (define (draw-example calculate-positions)
    (with-window (window -1 10 -1 10)
      (def t a-tree)
      (def positions (calculate-positions t))
      (draw (color "gray" (grid (pt 0 0) (pt 10 10) (pt 0 0) #:step 1))
            (draw-tree t positions))))

  (draw-example minimum-width-tree-positions)
  (draw-example simple-centered-tree-positions))


(require "node.rkt")

(define (draw-tree/nodes tree positions)
  (define (posn t) (hash-ref positions t))
  (def w (* 3 (current-node-size)))
  (define (pos->node-pos p)
    (defm (pt x y) p)
    (pt (* w x) (* (+ w 1) y)))
  (define (recur tree drawing)
    (def p (pos->node-pos (posn tree)))
    (def n (square-node p))
    (def d (draw drawing n (label-cnt (~a (element tree)) p)))
    (cond [(leaf? tree) d]
          [else         (draw d (for/draw ([c (children tree)])
                                          (def nc (square-node (pos->node-pos (posn c))))
                                          (draw (edge n nc up down)
                                                (recur c d))))]))
  (recur tree (draw)))

(set-curve-pict-size 300 300)
(with-window (window 0 6 -1 5)
  (define a-tree '(a (b (d) (e (h))) (c (f (i)) (g))))
  (draw-tree/nodes a-tree (simple-centered-tree-positions a-tree)))

#;(let ()
    (set-curve-pict-size 300 300)
    (define (draw-example calculate-positions)
      (with-window (window -1 10 -1 10)
        (def t '(a (b (c (d) 
                         (e) 
                         (f)
                         (i)
                         (j)))
                   (g)
                   (h)))
        (draw (color "gray" (grid (pt 0 0) (pt 10 10) (pt 0 0) 1))
              (draw-tree t (calculate-positions t)))))
    (beside (draw-example minimum-width-tree-positions)
            (draw-example simple-centered-tree-positions)))
