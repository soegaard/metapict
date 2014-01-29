#lang racket
(provide regular-polygon
         triangle
         square
         star)

; TODO: Make these polygones closed

(require math/flonum 
         "def.rkt" "structs.rkt" "pt-vec.rkt" "trans.rkt" "curve.rkt" "path.rkt")

(define (regular-polygon n 
                         #:inradius     [inradius #f] 
                         #:circumradius [circumradius #f] 
                         #:side         [side #f]
                         #:direction    [d (vec 0 1)])
  ; Use one of these: r inradius, R circumradius, s side length. The default is s=1.
  ; The direction gives the direction of the "first" point.
  ; The direction can be either a vec or an angle in radian.
  (define (s->R s) (* 1/2 s (flcscpix (/ 1. n))))
  (define (r->R s) (* 1/2 s (flsecpix (/ 1. n))))
  (def R (cond 
           [side         (s->R side)]
           [inradius     (r->R inradius)]
           [circumradius circumradius]
           [else         (s->R 1)]))
  (def rot (rotated (/ 2π n)))
  (def unit (cond [(number? d) (dir/rad d)]
                  [(vec? d)    (vec* (/ (len d)) d)]))
  (def A (pt+ origo (vec* R unit)))
  (curve*
   (for/list ([i (+ n 1)])
     (cond [(= i 0) (list A)]
           [else    (list -- ((rotated (* i (/ 2π n))) A))]))))


(define (triangle #:inradius [r #f] #:circumradius [R #f] 
                  #:side     [s #f] #:direction    [d (vec 1 1)])
  (regular-polygon 3 #:inradius r #:circumradius R #:side s #:direction d))

(define (square #:inradius [r #f] #:circumradius [R #f] 
                #:side     [s #f] #:direction    [d (vec 1 1)])
  (regular-polygon 4 #:inradius r #:circumradius R #:side s #:direction d))


(define (star p q #:circumradius[circumradius #f] #:side[s #f] #:direction[d (vec 0 1)])
  ; See http://mathworld.wolfram.com/StarPolygon.html
  ; p points, every qth point is connected
  (def R (cond [s (* s (/ (flsinpix (/ (- p (* 2 q)) (* 2. p))) 
                          (flsinpix (/ (* 2. q)p))))]
               [circumradius circumradius]
               [else         1/2]))
  (def unit (cond [(number? d) (dir/rad d)]
                  [(vec? d)    (vec* (/ (len d)) d)]))
  (def A (pt+ origo (vec* R unit)))
  (path*
   (cons A
         (let loop ([i q])
           (def B ((rotated (* i (/ 2π p))) A))
           (match (remainder i p)
             [0 (list -- B)]
             [_ (cons -- (cons B (loop (+ i q))))])))))


#;(define (triangle #:inradius     [inradius #f] 
                  #:circumradius [circumradius #f] 
                  #:side         [side #f])
  ; use one of these: r inradius, R circumradius, s side length
  ; default: s=1
  (define (s->R s) (* 1/2 (flcscpix (* 1/3 1.))))
  (define (r->R s) (* 1/2 (flsecpix (* 1/3 1.))))
  (def R (cond 
           [side         (s->R side)]
           [inradius     (r->R inradius)]
           [circumradius circumradius] 
           [else         (s->R 1)]))
  (def rot (rotated (/ 2π 3)))
  (def A (pt 0 R))
  (curve A -- (rot A) -- (rot (rot A)) -- A))



