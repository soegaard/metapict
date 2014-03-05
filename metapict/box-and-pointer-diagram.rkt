#lang racket
(require metapict)

;;; Box and Pointer Diagrams

; This shows how to draw classical box and pointer diagrams 
; in SICP style. The call (draw-box-and-pointer-diagram v)
; will draw the value v using box and pointers.
; The function works on both mutable and immutable cons cells.

; As is the code doesn't compute the extent of the drawing,
; so you need to modify the x- and y-range if your 
; data structure gets too large:

(defv (xmin xmax ymin ymax) (values -10 10 -10 10))

; Patches that automatically compute the ranges are welcome.

; The size of the arrow heads:
(ahlength (px 8))
; NB: Due to a (temporary) bug in the drawing of arrow heads,
;     make sure the size of the x-range and the y-range
;     are of equal size (otherwise the arrows get distorted).

(define (draw-null-box upper-left)
  ; null is drawn as a crossed over box
  (def ul upper-left)
  (draw (rectangle ul dr)
        (curve (pt+ ul down) -- (pt+ ul right))))


(define (draw-value v)
  ; values are simply displayed with ~a
  (text (~a v)))

(def dr   (vec+ down right))
(def dr/2 (vec* 1/2 dr))

(define (draw-cdr upper-left d recur)  
  (def ul upper-left)
  (def dm (pt+ ul right dr/2)) ; middle of cdr box
  (match d
    ; if null, the value d (from a cdr) is drawn as a crossed over rectangle
    [(list) (draw-null-box (pt+ ul right))]
    ; otherwise  i) use recur to draw d placed 3 units to the right of the cons cell
    [_ (match (recur (pt+ ul (vec* 3 right)) d)
         ;      ii) connect the cdr part of the cons cell to the value d
         [(? pt? ul-d) (draw-arrow (curve dm right .. (pt+ ul-d (vec 1/2 0)) down))]
         [d-pict       (draw (draw-arrow (curve dm -- (pt+ dm (vec* 3/2 right))))
                             d-pict)])]))

(define (depth v)
  (def seen-pairs (make-hasheq))
  (define (seen! p) (hash-set! seen-pairs p #t))
  (define (seen? p) (hash-ref  seen-pairs p #f))
  (define (recur v)
    (cond [(seen? v) 0]
          [else      (seen! v)
                     (match v
                       [(or (cons a d) (mcons a d)) (+ (recur a) (recur d))]
                       [(list)     1]
                       [_ 2])]))
  (recur v))

(define (draw-car upper-left a depth-d recur)
  (def ul upper-left)
  (def am (pt+ ul dr/2))
  (match a
    [(list) (draw-null-box ul)]
    [_ (match (recur (pt+ ul (vec* (+ depth-d 1) down)) a)
         [(? pt? ul-a) 
          (draw-arrow (curve am down ..
                             (pt+ am (vec 0 -1/2)) down .. 
                             (pt+ ul-a (vec 0 -1/2)) right))]
         [a-pict       
          (draw (draw-arrow (curve am -- (pt+ am (vec* (+ depth-d 1/2) down))))
                a-pict)])]))

(define (draw-box-and-pointer-diagram v)
  (def seen-pairs (make-hasheq))
  (define (seen! p ul) (hash-set! seen-pairs p ul))
  (define (seen? p) (hash-ref  seen-pairs p #f))
  (define (recur upper-left v)
    (def ul upper-left)
    (cond
      [(seen? v) (hash-ref seen-pairs v)]
      [else      
       (unless (or (number? v) (symbol? v)) ; shared numbers and symbols clutters the diagram
         (seen! v ul))
       (match v
         [(list)       
          (draw-null-box ul)]
         [(or (cons a d) (mcons a d))   
          (def depth-d (depth d))
          (draw (rectangle ul             (pt+ ul dr))
                (rectangle (pt+ ul right) (pt+ ul right dr))
                (draw-cdr ul d recur)
                (draw-car ul a depth-d recur))]
         [_ 
          (label-cnt (~a v) (pt+ ul dr/2))])]))
  (recur (pt+ (pt xmin ymax) right down) v))


               
(set-curve-pict-size 400 400)
(curve-pict-window (window xmin xmax ymin ymax))
(def gray-grid (color "gray" (grid (pt xmin ymin) (pt xmax ymax) (pt 0 0) 1)))

(draw gray-grid
      (draw-box-and-pointer-diagram
       (list "1" "2" (list "3" "4") 5)))

(draw gray-grid
      (draw-box-and-pointer-diagram 
       (list 2 (list 1) (list 3 (list 4 5) 6) 7)))

(draw gray-grid
      (draw-box-and-pointer-diagram 
       (shared ([a (cons 1 a)]) a)))

(draw-box-and-pointer-diagram 
   (shared ([a (cons 1 a)]) (list a 'b a 'c a)))

(draw-box-and-pointer-diagram 
   (shared ([a (cons 1 a)]) (list a 'b (list 1 "foo" a "foo") a 'c a)))

(let ()
  (local-require compatibility/mlist)
  (define l (mlist 1 2 3))
  (set-mcar! (mcdr l) (mcdr (mcdr l)))
  (draw-box-and-pointer-diagram l))



    