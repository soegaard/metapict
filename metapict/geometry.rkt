#lang racket
(require metapict)
(module+ test (require rackunit))

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


; new-line : point point boolean boolean -> line
(define (new-line p q [extend-p #t] [extend-q #t])
  (when (equal? p q)
    (error 'new-line "the points need to be different"))
  (line p q extend-p extend-q))

; line-abc : line -> (values real real real)
;   return a, b, and, c such that ax+by+c=0
;   is an equation for the line through p and q
(define (line-abc l)
  (match-define (line (pt px py) (pt qx qy) _ _) l)
  (define a (- qy py))
  (define b (- px qx))
  (define c (- (* py qx) (* px qy)))
  (values a b c))

; unit : vec -> vec
;   return unit vector with same direction as v
(define (unit v) 
  (vec* (/ (len v)) v))

; line-direction-vector : line -> vec
;   unit vector parallel to the line l
;   [Asymptote: line-u]
(define (line-direction-vector l) 
  (match-define (line p q _ _) l)
  (unit (pt- q p)))

; line-normal-vector : line -> vec
;   unit vector ortogonal to the line l 
;   [Asymptote: line-v]
(define (line-normal-vector l) 
  (rot90 (line-direction-vector l)))

; line-slope : line -> real
;   compute slope for line l.
;   if the line is vertical return +inf.0
(define (line-slope l)
  (match-define (line (pt x1 y1) (pt x2 y2) _ _) l)
  (if (= x1 x2)
      +inf.0
      (/ (- y2 y1)
         (- x2 x1))))

(module+ test 
  (check-equal? (line-slope (new-line (pt -1 2) (pt 1 4))) 1)
  (check-equal? (line-slope (new-line (pt -1 2) (pt -1 4))) +inf.0))

; line-origin : line -> real
;   return y when x=0
(define (line-origin l) 
  (match-define (line (pt x y) _ _ _) l)
  (define a (line-slope l))
  (- y (* a x)))

(module+ test (check-equal? (line-origin (new-line (pt -1 2) (pt 1 4))) 3))

; dist-pt-to-line : pt line -> real
;   distance from point to line
(define (dist-pt-to-line p l)
  (defm (pt x y) p)
  (defv (a b c) (line-abc l))
  (/ (abs (+ (* a x) (* b y) c))
     (sqrt (+ (sqr a) (sqr b)))))

(module+ test
  (check-equal? (dist-pt-to-line (pt 1 0) (new-line (pt 0 0) (pt 0 1))) 1)
  (check-true   (<= (abs (- (dist-pt-to-line (pt 0 0) (new-line (pt 0 1) (pt 1 0))) 
                            (/ (sqrt 2) 2)))
                    1e-14)))

; line/line-intersection : line line -> (or pt +inf.0)
;   return the intersection between line1 and line2
;   if there are none, return (pt +inf.0 +inf.0)
(define (line/line-intersection line1 line2)
  (match-define (line (pt x1 y1) (pt x2 y2) l1 r1) line1)
  (match-define (line (pt x3 y3) (pt x4 y4) l2 r2) line2)
  (define denom      (- (* (- x1 x2) (- y2 y4))
                        (* (- y1 y2) (- x3 x4))))
  (define d12 (- (* x1 y2) (* y1 x2)))
  (define d34 (- (* x3 y4) (* y3 x4)))
  (define numeratorx (- (* d12 (- x3 x4)) (* d34 (- x1 x2))))
  (define numeratory (- (* d12 (- y3 y4)) (* d34 (- y1 y2))))
  (if (zero? denom)
      (pt +inf.0 +inf.0)
      (pt (/ numeratorx denom) (/ numeratory denom))))

(module+ test
  (check-equal? (line/line-intersection (new-line (pt 0 0) (pt 2 2))
                                        (new-line (pt 0 2) (pt 2 0)))
                (pt 1 1)))

;;; CIRCLE

(struct circle: (c r l) #:transparent)
; c = center
; r = radiues
; l = line used if r=+inf.0

(define point? pt?)

; new-circle : pt number -> circle
; new-circle : pt pt     -> circle
; new-circle : pt pt pt  -> circle
;   create circle either from
;     i) center and radius
;    ii) center and point on periphery
;   iii) three points on the circle
(define new-circle
  (match-lambda*
   [(list (? point? c) (? number? r))
    ; center and radius
    (circle: c r #f)]
   [(list (? point? c) (? point? a))
    (define d (dist c a))
    (unless (positive? d) 
      (error 'new-circle "the two points must be different"))
    (circle: c d #f)]
   [(list (? point? a) (? point? b) (? point? c))
    ; circle through points a, b, and, c
    (circum-circle a b c)]
   [(list _ _) (error 'new-circle)]))

; circum-center : pt pt pt -> circle
;   given three non collinear points, return
;   center of the circle that passes through the points
(define (circum-center a b c)
  (match-define (pt ax ay) a)
  (match-define (pt bx by) b)
  (match-define (pt cx cy) c)
  (define d (* 2 (+ (* ax (- by cy)) (* bx (- cy ay)) (* cx (- ay by)))))
  (define ux (/ (+ (* (+ (sqr ax) (sqr ay)) (- by cy))
                   (* (+ (sqr bx) (sqr by)) (- cy ay))
                   (* (+ (sqr cx) (sqr cy)) (- ay by)))
                d))
  (define uy (/ (+ (* (+ (sqr ax) (sqr ay)) (- cx bx))
                   (* (+ (sqr bx) (sqr by)) (- ax cx))
                   (* (+ (sqr cx) (sqr cy)) (- bx ax)))
                d))
  (pt ux uy))
 
(module+ test (check-equal? (circum-center (pt -1 0) (pt 0 1) (pt 1 0)) (pt 0 0)))

; circum-circle : pt pt pt -> circle
;   given three points, return the circle
;   that passes throught the three points
(define (circum-circle a b c)
  (define u (circum-center a b c))
  (new-circle u (dist u a)))


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


