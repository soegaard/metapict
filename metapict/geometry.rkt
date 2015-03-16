#lang racket
(require metapict)
;;; WORK IN PROGRESS

; The goal is to provide functionality similar to
; geometry.asy from Asymptote.


;;; LINES

; http://www.piprime.fr/files/asymptote/geometry/modules/geometry.asy.index.type.html#struct line

(struct line (p q l r) #:transparent)
; p and q are points. l and r are booleans. 

; a, b, and c are reals. u and v are vecs.
;    ax+by+c=0
;    u = unit vector in the direction from p to q
;    v = normal vector
;    

; (line p q #t #t) represents a line through points p and q
; (line p q #t #f) represents a ray from q through p
; (line p q #f #t) represents a ray from p through q
; (line p q #f #f) represents a line segment from p to q

(define (new-line p q [extend-p #t] [extend-q #t])
  (line p q extend-p extend-q))

(define (line-abc l)
  ; return a, b, and, c such that ax+by+c=0
  ; is an equation for the line through p and q
  (match-define (line p q _ _) l)
  (match-define (vec a b) (rot90 (pt- q p)))
  (match-define (pt x0 y0) p)
  (define c (- (+ (* a x0) (* b y0))))
  (values a b c))

(define (unit v) ; unit vector with same direction as v
  (vec* (/ (len v)) v))

(define (line-u l) ; unit vector parallel to l
  (match-define (line p q _ _) l)
  (unit (pt- q p)))

(define (line-v l) ; unit vector perpendicular to l
  (rot90 (line-u l)))

(define (line-slope l)
  (match-define (line (pt x1 y1) (pt x2 y2) _ _) l)
  (when (= x1 x2) (error 'line-slope "line is vertical"))
  (/ (- y2 y1)
     (- x2 x1)))

(define (line-origin l) ; y when x=0
  (match-define (line (pt x y) _ _ _) l)
  (define a (line-slope l))
  (- y (* a x)))



(define (ray p q)      (line p q #f #t))
(define (seg p q)      (line p q #f #f))
