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

(define (line/line-intersection line1 line2)
  (match-define (line (pt x1 y1) (pt x2 y2) l1 r1) line1)
  (match-define (line (pt x3 y3) (pt x4 y4) l2 r2) line2)
  (define denom (- (* (- x1 x2) (- y2 y4))
                   (* (- y1 y2) (- x3 x4))))
  (define numeratorx (- (* (- (* x1 y2) (* y1 x2)) (- x3 x4))
                        (* (- (* x3 y4) (* y3 x4)) (- x1 x2))))
  (define numeratory (- (* (- (* x1 y2) (* y1 x2)) (- y3 y4))
                        (* (- (* x3 y4) (* y3 x4)) (- y1 y2))))
  (if (zero? denom)
      (pt +inf.0 +inf.0)
      (pt (/ numeratorx denom) (/ numeratory denom))))

#;(check-equal? (line/line-intersection (new-line (pt 0 0) (pt 2 2))
                                        (new-line (pt 0 2) (pt 2 0)))
                (pt 1 1))

; (line p q #t #t) represents a line through points p and q
; (line p q #t #f) represents a ray from q through p
; (line p q #f #t) represents a ray from p through q
; (line p q #f #f) represents a line segment from p to q


(define (new-line p q [extend-p #t] [extend-q #t])
  (when (equal? p q)
    (error 'new-line "the points need to be different"))
  (line p q extend-p extend-q))

(define (line-abc l)
  ; return a, b, and, c such that ax+by+c=0
  ; is an equation for the line through p and q
  (match-define (line (pt px py) (pt qx qy) _ _) l)
  (define a (- qy py))
  (define b (- px qx))
  (define c (- (* py qx) (* px qy)))
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
  (if (= x1 x2)
      +inf.0
      (/ (- y2 y1)
         (- x2 x1))))

(define (line-origin l) ; y when x=0
  (match-define (line (pt x y) _ _ _) l)
  (define a (line-slope l))
  (- y (* a x)))

; dist-pt-to-line : pt line -> real
;   distance from point to line
(define (dist-pt-to-line p l)
  (defm (pt x y) p)
  (defv (a b c) (line-abc l))
  (/ (abs (+ (* a x) (* b y) c))
     (sqrt (+ (sqr a) (sqr b)))))

;;; PARABOLAS

(struct parabola (f v) #:transparent)
; f and v are points
; f is the focus 
; v is the vertex 
; The parabola consists of all points whose distances to f and v are the same

; The focal parameter a is the distance from the from the vertex to the focus.

(define (parabola-focal p)
  (match-define (parabola f v) p)
  (dist f v))

(define (directrix p)
  (match-define (parabola f v) p)
  (define fv (pt- v f))
  (define P (pt+ v fv))
  (define Q (pt+ P (rot90 fv)))
  (new-line P Q))

(define (excentricity x)
  (match x
    [(parabola f v) 1]
    ; TODO case for hyperbola
    ; TODO case for ellipse
    [_ (error)]))

;;; SIDE (in a triangle)

(struct side (n t) #:transparent)
; n = 0 or 1 means [AB],
; n = -1     means [BA],
; n =  2     means [BC]
; n = -2     means [CB]
; t = triangle

;;; GENERAL

(define (midpoint p1 p2)
  (med 1/2 p1 p2))

(define (distance o1 o2)
  (match* {o1 o2}
    [{(? pt?) (? pt?)}   (dist o1 o2)]
    [{(? pt?) (? line?)} (dist-pt-to-line o1 o2)]
    [{(? line?) (? pt?)} (dist-pt-to-line o2 o1)]
    [{_ _} (error 'distance)]))

(define (same-side? m n l)
  ; are the points m and n on the same side of the line l?
  (defm (line (pt x1 y1) (pt x2 y2) _ _) l)
  (defm (pt ax ay) m)
  (defm (pt bx by) n)
  (positive? (* (+ (* (- y1 y2) (- ax x1))
                   (* (- x2 x1) (- ay y1)))
                (+ (* (- y1 y2) (- bx x1))
                   (* (- x2 x1) (- by y1))))))


(define (area o)
  (match o
    ; [(? triangle?) (triangle-area o)]
    [_ (error 'area (~a "got: " o))]))


