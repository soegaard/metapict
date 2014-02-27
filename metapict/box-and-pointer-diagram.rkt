#lang racket
(require metapict)

(ahlength (px 8))

(defv (xmin xmax ymin ymax) (values -10 10 -10 10))

(def dr   (vec+ down right))
(def dr/2 (vec* 1/2 dr))

(define (draw-null-box upper-left)
  (def ul   upper-left)
  (draw (rectangle ul dr)
        (curve (pt+ ul down) -- (pt+ ul right))))

(define (draw-value v)
  (text (~a v)))

(define (draw-cdr upper-left d recur)
  (def ul upper-left)
  (def dm (pt+ ul right dr/2))
  (match d
    [(list) (draw-null-box (pt+ ul right))]
    [_      (match (recur (pt+ ul (vec* 3 right)) d)
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
                       [(cons a d) (+ (recur a) (recur d))]
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
         [(list)       (draw-null-box ul)]
         [(cons a d)   (def depth-d (depth d))
                       (draw (rectangle ul             (pt+ ul dr))
                             (rectangle (pt+ ul right) (pt+ ul right dr))
                             (draw-cdr ul d recur)
                             (draw-car ul a depth-d recur))]
         [_            (label-cnt (~a v) (pt+ ul dr/2))])]))
  (recur (pt+ (pt+ (pt xmin ymax) right) down) v))


               
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




    