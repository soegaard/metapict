#lang racket
(require metapict)

(ahlength (px 8))

(defv (xmin xmax ymin ymax) (values -8 8 -8 8))

(def dr   (vec+ down right))
(def dr/2 (vec* 1/2 dr))

(define (draw-null-box upper-left)
  (def ul   upper-left)
  (draw (rectangle ul dr)
        (curve (pt+ ul down) -- (pt+ ul right))))

(define (draw-value v)
  (text (~a v)))

(define (draw-box-and-pointer-diagram v)
  (define (recur upper-left v)
    (def ul   upper-left)
    (match v
      [(list)     
       (draw-null-box ul)]
      [(cons a d) 
       (def am (pt+ ul       dr/2))
       (def dm (pt+ ul right dr/2))
       (draw (rectangle ul             (pt+ ul dr))
             (rectangle (pt+ ul right) (pt+ ul right dr))
             (draw-arrow (curve am -- (pt+ am (vec* 3/2 down))))
             (recur (pt+ ul (vec* 2 down)) a)
             (cond [(null? d) (draw-null-box (pt+ ul right))]
                   [else      (draw (draw-arrow (curve dm -- (pt+ dm (vec* 3/2 right))))
                                    (recur (pt+ ul (vec* 3 right)) d))]))]
      [_ (label-cnt (~a v) (pt+ ul dr/2))]))
  (recur (pt xmin ymax) v))

(def val (list "1" "2" (list "3" "4") 5))
               
(set-curve-pict-size 400 400)
(with-window (window xmin xmax ymin ymax)
  (draw (color "gray" (grid (pt xmin ymin) (pt xmax ymax) (pt 0 0) 1))
        (draw-box-and-pointer-diagram val)))
    