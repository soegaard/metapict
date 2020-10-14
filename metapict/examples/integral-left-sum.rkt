#lang racket
;;;
;;; EXAMPLE
;;;   This example illustrations numeric integration 
;;;   with left sums.
;;;

; todo: fix "bleeding" by reusing the path of the graph
; with cut-before and cut-after rather than recomputing the graph.

; Note: The cut-before and cut-after solution needs to
;       handle the case where x=a and x=b doesn't intersect the 
;       graph g inside the window.
;       One solution is to let x=a and x=b extend from the
;       min to the max value of f.

(require "../metapict.rkt")

(def devw 600)
(def devh 600)
(def dev-ratio (/ devh devw))  ; r=Δy/Δx thus Δy=r*Δx 

(curve-pict-width  devw)
(curve-pict-height devh)

(def xmin -1.5)
(def xmax  5)
(def ymin -1)
(def ymax  (+ ymin (* dev-ratio (- xmax xmin))))

(def x-axis (curve (pt xmin 0) -- (pt xmax 0)))
(def y-axis (curve (pt 0 ymin) -- (pt 0 ymax)))

(define f (λ (x) (+ (* 1/4 (- x 0) (- x 2) (- x 4)) 2)))
; (define (f x) x)

(def n 50) ; number of points used to draw graph

(def a -0.5)
(def b  4.5)

(def  A (pt a (f a)))
(def  B (pt b (f b)))
(def xA (pt a 0))
(def xB (pt b 0))

(define (sort-points ps) (sort ps (λ(p q) (< (pt-x p) (pt-x q)))))

(define (add-points qs ps from to) ; add points in qs to ps only if they are in [xmin;xmax]
  (define (add-point p ps) 
    (cond [(and (<= from (pt-x p) to) (not (member p ps pt=))) (cons p ps)]
          [else ps]))
  (sort-points (foldl add-point ps qs)))

(define (graph-points xmin xmax n f)
  (def Δx (/ (- xmax xmin) n))
  (for/list ([x (in-range xmin (+ xmax Δx) Δx)])
    (pt x (f x))))

(define (minmax ps keyf)
  (for/fold ([ymin +inf.0] [ymax -inf.0]) ([p ps]) 
    (def y (keyf p))
    (values (min ymin y) (max ymax y))))

(define (graph ps)
  (curve* (add-between ps ..)))

(define (left-rectangles ps)
  (match ps
    [(list* (pt x y) (pt x1 y1) ps)
     (cons (curve (pt x 0) -- (pt x y) -- (pt x1 y) -- (pt x1 0) -- cycle)
           (left-rectangles (cons (pt x1 y1) ps)))]
    [_ '()]))

(def m 5) ; number of rectangles

(with-window (window xmin xmax ymin ymax)
  (ahlength (px 8))
  (def rs (graph-points a b m f))     ; for rectangles  
  (def ps (add-points (append (list A B) rs)
                      (graph-points xmin xmax n f) ; for graph
                      xmin xmax))
  (def g (graph ps))
  (defv (gmin gmax) (minmax ps pt-y))
  (defv (-yinf +yinf) (values (* 2 (min ymin gmin)) (* 2 (max ymax gmax))))
  (def x=a (curve (pt a -yinf) -- (pt a +yinf)))
  (def x=b (curve (pt b -yinf) -- (pt b +yinf)))
  ; x=a and x=b needs to extend beyond the window in order to intersect g
  (def g-xmin-to-a (cut-after g x=a))
  (def g-a-to-b    (cut-after (cut-before g x=a) x=b))
  (def g-b-to-xmax (cut-before g x=b))
  (def region (curve-append (curve xA -- A) (curve-append g-a-to-b (curve B -- xB -- xA))))
  (draw (label (~a "Left sum n=" m) (pt (/ (+ xmin xmax) 2) ymax) (bot))
        (color "lightblue" (fill region))
        (draw-arrow x-axis)
        (draw-arrow y-axis)        
        ; (draw g-xmin-to-a g-a-to-b g-b-to-xmax)
        (draw* (for/list ([r (left-rectangles rs)]) 
                 (draw (color (change-alpha "gray" 0.5) (fill r)) 
                       r)))
        (curve xA -- A)
        (curve xB -- B)
        (color "red" (draw g))
        (penscale 8 (draw* (map draw rs)))))

