#lang racket
(require metapict compatibility/mlist)

;;; Box and Pointer Diagrams

; This shows how to draw classical box and pointer diagrams 
; in SICP style. The call (draw-box-and-pointer-diagram v)
; will draw the value v using boxes and pointers.
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

(define (depth v)
  (def seen-pairs (make-hasheq))
  (define (seen! p) (hash-set! seen-pairs p #t))
  (define (seen? p) (hash-ref  seen-pairs p #f))
  (define (recur v)
    (cond [(seen? v) 0]
          [else      (seen! v)
                     (match v
                       [(or (cons a d) (mcons a d)) (+ (recur a) (recur d))]
                       [(list) 1]
                       [_      2])]))
  (recur v))

(define (draw-null-box upper-left)
  ; null is drawn as a crossed over box
  (def ul upper-left)
  (draw (rectangle ul dr)
        (curve (pt+ ul down) -- (pt+ ul right))))

(define (embeddable-value? v)
  #f
  ; an embeddable value is drawn inside a car or cdr box
  #;(or (and (number? v) (<= (abs v) 100))
      (char? v)))

(define (draw-embeddable-value v cnt)
  ; small value centered on cnt
  (draw (label-cnt (~v v) cnt)))

(define (draw-value v)
  ; values are simply displayed with ~v
  (text (~v v)))

(define (atomic-value? v)
  ; atomic values are drawn direcly below their cell,
  (or (number? v)
      (string? v)
      (symbol? v)
      (char? v)))


(def dr   (vec+ down right))
(def dr/2 (vec* 1/2 dr))

(define (draw-cdr upper-left d recur)  
  (def ul upper-left)
  (def dm (pt+ ul right dr/2)) ; middle of cdr box
  (match d
    ; if null, the value d (from a cdr) is drawn as a crossed over rectangle
    [(list) (draw-null-box (pt+ ul right))]
    ; draw embeddable values inside the box
    [(? embeddable-value? a) (draw-embeddable-value a dm)]
    ; otherwise  i) use recur to draw d placed 3 units to the right of the cons cell
    [_ (match (recur (pt+ ul (vec* 3 right)) d)
         ;      ii) connect the cdr part of the cons cell to the value d
         [(? pt? ul-d) (draw-arrow (curve dm right .. (pt+ ul-d (vec 1/2 0)) down))]
         [d-pict       (draw (draw-arrow (curve dm -- (pt+ dm (vec* 3/2 right))))
                             d-pict)])]))

(define (draw-car upper-left a depth-d recur)
  (def ul upper-left)
  (def am (pt+ ul dr/2))
  (match a
    [(list) (draw-null-box ul)]
    [(? embeddable-value? a) (draw-embeddable-value a am)]
    [_ (def offset (if (atomic-value? a) 1/2 (+ depth-d 0)))
       (match (recur (pt+ ul (vec* (+ offset 1) down)) a)
         [(? pt? ul-a) ; got upper-left corner of already drawn value
          ; draw arrow, but first is it upwards or downwards?
          (if (positive? (dot (pt- ul-a ul) up))
              (draw-arrow (curve am                      up ..
                                 (pt+ am   (vec 0  1/2)) up .. 
                                 (pt+ ul-a (vec 0 -1/2)) right))
              (draw-arrow (curve am                      down ..
                                 (pt+ am   (vec 0 -1/2)) down .. 
                                 (pt+ ul-a (vec 0 -1/2)) right)))]
         [a-pict       
          (draw (draw-arrow (curve am -- (pt+ am (vec* (+ offset 1/2) down))))
                a-pict)])]))

(define (draw-cons-cell upper-left v recur)
  (def ul upper-left)
  (match v
    [(or (cons a d) (mcons a d))
     (draw (rectangle ul             (pt+ ul dr))
           (rectangle (pt+ ul right) (pt+ ul right dr))
           (draw-cdr ul d recur)
           (draw-car ul a (depth d) recur))]))

(define (draw-label ul v labels)
  ; Labels is a hash table from that maps cons cells to be labelled into 
  ; strings, picts or one-argument procedures mapping a point (upper-left corner
  ; of the cons cell) into a label
  (match (hash-ref labels v #f)
    [(? string? l)    (label-top l ul)]
    [(? pict? l)      (label-top l ul)]
    [(? procedure? f) (f ul)]
    [#f               (blank)]
    [_ (error 'draw-label (~a "expect label, pict or string, got: " v))]))

(define (draw-box-and-pointer-diagram 
         v #:upper-left [upper-left (pt+ (pt xmin ymax) right down)]
           #:labels     [labels (hash)])
  ; pairs already seen will not be drawn again
  (def seen-pairs (make-hasheq))
  (define (seen! p ul) (hash-set! seen-pairs p ul))
  (define (seen? p) (hash-ref  seen-pairs p #f))
  (define (recur ul v)
    ; draw the value v, the upper-left is at the position ul
    (cond
      [(seen? v) (hash-ref seen-pairs v)]
      [else  
       (unless (atomic-value? v) ; only share compound values (to avoid clutter)
         (seen! v ul))
       (draw (draw-label ul v labels)
             (match v
               [(list)                      (draw-null-box ul)]
               [(or (cons a d) (mcons a d)) (draw-cons-cell ul v recur)]
               [_ (label-cnt (~a v) (pt+ ul dr/2))]))]))
  (recur upper-left v))
               
(set-curve-pict-size 400 400)
(curve-pict-window (window xmin xmax ymin ymax))
(def gray-grid (color "gray" (grid (pt xmin ymin) (pt xmax ymax) (pt 0 0) #:step 1)))

(draw gray-grid
      (draw-box-and-pointer-diagram
       (list "1" "2" (list "3" "4") 5)))

(draw gray-grid
      (draw-box-and-pointer-diagram 
       (list 2 (list 1) (list 3 (list 4 5) 6) 7)))

(draw gray-grid
      (shared ([a (cons 1 a)]) 
        (draw-box-and-pointer-diagram 
         a #:labels (hash a "a"))))

(draw-box-and-pointer-diagram 
   (shared ([a (cons 1 a)]) (list a 'b a 'c a)))

(draw-box-and-pointer-diagram 
   (shared ([a (cons 1 a)]) (list a 'b (list 1 "foo" a "foo") a 'c a)))

(let ()
  (local-require compatibility/mlist)
  (define l (mlist 1 2 3))
  (set-mcar! (mcdr l) (mcdr (mcdr l)))
  (draw-box-and-pointer-diagram l))

(margin 
   5 (draw (shared ([a (cons 1 a)]
                    [b (cons c a)]
                    [c (list 2)])
             (draw-box-and-pointer-diagram 
              b #:labels (hash a "a" b "b" c "c")))))


; todo: this last example draws a and c ontop of each other - why?
(margin 
   5 (draw (shared ([a (cons 1 a)]
                    [b (cons c a)]
                    [c (list 2)]
                    [d (list a b c d)])
             (draw-box-and-pointer-diagram 
              d #:labels (hash a "a" b "b" c "c" d "d")))))
