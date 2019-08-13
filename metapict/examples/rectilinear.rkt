#lang racket
(require metapict metapict/curve)

(def /- '/-)
(def -/ '-/)
(def --++ '--++)

(define (handle/- p0 p1 more)
  (defm (pt x0 _) p0)
  (defm (pt _ y1) p1)
  (list* p0 -- (pt x0 y1) -- p1 more))

(define (handle-/ p0 p1 more)
  (defm (pt _ y0) p0)
  (defm (pt x1 _) p1)
  (list* p0 -- (pt x1 y0) -- p1 more))

(define (handle--++ p0 v1 more)
  (defm (pt x0 y0) p0)
  (defm (or (pt x1 y1) (vec x1 y1)) v1)
  (list* p0 -- (pt+ p0 (vec x1 y1)) more))

(define (Curve . ds)
  (define simple-path-description
    (let loop ([ds ds])
      (match ds
        [(list* (? pt? p0) '/- (? pt? p1) more)
         (loop (handle/- p0 p1 more))]
        [(list* (? pt? p0) '-/ (? pt? p1) more)
         (loop (handle-/ p0 p1 more))]
        [(list* (? pt? p0) '--++ (or (? pt? p1) (? vec? p1)) more)
         (loop (handle--++ p0 p1 more))]
        [(cons p ps)
         (cons p (loop ps))]
        ['()
         '()])))
  (apply make-curve simple-path-description))


(draw-arrow (Curve (pt 0 0) /- (pt 1/2 1/2) -/ (pt 1 -1)))
(draw-arrow (Curve (pt 0 0) --++ (vec 1/2 1)  --++ (vec 1/2 -1)))


