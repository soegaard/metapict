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
    [_      (draw (draw-arrow (curve dm -- (pt+ dm (vec* 3/2 right))))
                  (recur (pt+ ul (vec* 3 right)) d))]))

(define (depth v)
  (match v
    [(cons a d) (+ (depth a) (depth d))]
    [(list)     1]
    [_ 2]))

(define (draw-box-and-pointer-diagram v)
  (define (recur upper-left v)
    (def ul upper-left)
    (match v
      [(list)     
       (draw-null-box ul)]
      [(cons a d) 
       (def am (pt+ ul       dr/2))
       (def depth-d (depth d))
       (draw (rectangle ul             (pt+ ul dr))
             (rectangle (pt+ ul right) (pt+ ul right dr))
             (draw-cdr ul d recur)
             (draw-arrow (curve am -- (pt+ am (vec* (+ depth-d 1/2) down))))
             (recur (pt+ ul (vec* (+ depth-d 1) down)) a)
             )]
      [_ (label-cnt (~a v) (pt+ ul dr/2))]))
  (recur (pt xmin ymax) v))

(def val (list "1" "2" (list "3" "4") 5))
               
(set-curve-pict-size 400 400)
(with-window (window xmin xmax ymin ymax)
  (draw (color "gray" (grid (pt xmin ymin) (pt xmax ymax) (pt 0 0) 1))
        (draw-box-and-pointer-diagram val)))

(set-curve-pict-size 400 400)
  (with-window (window xmin xmax ymin ymax)
  (draw (color "gray" (grid (pt xmin ymin) (pt xmax ymax) (pt 0 0) 1))
        (draw-box-and-pointer-diagram (list 2 (list 1) (list 3 (list 4 5) 6) 7))))
    
    