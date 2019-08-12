#lang racket/base
;;;
;;; LINES, RAYS, AND SEGMENTS
;;;

;; This module provides functions work with lines, rays and segments.
;; The goal is to provide functionality similar to geometry.asy from Asymptote.


;;; Functions and structs

; (struct line (p q l r) #:transparent)        ; p, q points;  l,r booleans
;   Here p and q are points in logical coordinates
;   and l and r are booleans.

; (line p q #t #t) represents a line through points p and q
; (line p q #f #f) represents a line segment from p to q
; (line p q #t #f) represents a ray from q through p
; (line p q #f #t) represents a ray from p through q

; A line can conceptually be represented as an equation of the form
;    ax + by + c = 0
; where a, b and c are real numbers.

; In the code the letters u and v are vectors and stand for:

;    u = unit vector in the direction from p to q
;    v = normal vector


; new-line              : p q [l r] -> line    ; create new line, ray, or, segment
; line-abc : line -> (values real real real)   ; a, b, and, c such that ax+by+c=0
;                                                is an equation for the line through p and q

; unit                   : vec       -> vec     ; unit vector with same direction as v

; line-direction-vector  : line      -> vec     ; unit vector parallel  to the line l
; line-normal-vector     : line      -> vec     ; unit vector ortogonal to the line l 
; line-slope             : line      -> real    ; slope for line l (+inf.0 when vertical)
; line-origin            : line      -> real    ; y when x=0
; dist-pt-to-line        : pt line   -> real    ; distance from point to line

; line/line-intersection : line line -> (or pt +inf.0)  ; intersection between line1 and line2 
;                                                         if there are none, return (pt +inf.0 +inf.0)
; line/quadric-intersection : A B a b c d e f g -> ...  ; intersection points between the line AB and


;;; WORK IN PROGRESS
; TODO: test intersections between line and quadrics


(require racket/list racket/match racket/format (except-in math permutations)
         metapict metapict/structs)

(require 
         "parameters.rkt" "window.rkt" "pt-vec.rkt")

(module+ test (require rackunit))

(define (quadratic-solutions* a b c)
  ; quadratic-solutions from racket/math doesn't handle the case a=0
  (if (= a 0) 
      (list (/ (- c) b))
      (quadratic-solutions a b c)))


;;; LINES, RAYS, AND SEGMENTS

; http://www.piprime.fr/files/asymptote/geometry/modules/geometry.asy.index.type.html#struct line

; (struct line (p q l r) #:transparent)
; p and q are points. l and r are booleans. 

; a, b, and, c are reals. u and v are vecs.
;    ax+by+c=0
;    u = unit vector in the direction from p to q
;    v = normal vector

; (line p q #t #t) represents a line through points p and q
; (line p q #f #f) represents a line segment from p to q
; (line p q #t #f) represents a ray from q through p
; (line p q #f #t) represents a ray from p through q

; new-line : point point boolean boolean -> line
(define (new-line p q [extend-p #t] [extend-q #t])
  (when (equal? p q) (error 'new-line "the points need to be different"))
  (line p q extend-p extend-q))

(define (line-intersection l1 l2)
  ; this handles all combinations of line, ray and segment intersections
  (defm (line p1 q1 a1 b1) l1)
  (defm (line p2 q2 a2 b2) l2)

  (def  v1 (pt- q1 p1))
  (def  v2 (pt- q2 p2))
  ; If the two segments intersect, there exists s and t 
  ; such that:
  ;     p1 +t v1 = p2 + s v2
  ;      p1 - p2 = s v2 - t v1
  ;            P = s v2 - t v1
  (def P (pt- p1 p2))

  ; Isolate t by taking the inner product with v2^
  ; P * v2^ = (s v2)*v2^ - t (v1 * v2^)
  ; P * v2^ =            - t (v1*v2^)
  ; t = - (P * v2^) / (v1 * v2^)
  (def v2^ (rot90 v2))

  ; Isolate s by taking the inner product with v1^
  ; P * v1^ = (s v2)*v1^ - t (v1 * v1^)
  ; P * v1^ = (s v2)*v1^
  ; s = P*v1^ / v2*v1^
  (def v1^ (rot90 v1))

  ; The denominators are:  
  (def d12 (dot v1 v2^))
  (def d21 (dot v2 v1^))
  (cond
    ; better safe than sorry, so we check both
    [(or (zero? d12) (zero? 21))
     ; if the lines are parallel we end up here
     #f]
    [else
     (def t (/ (- (dot P v2^)) d12))
     (def s (/ (dot P v1^) d21))
     ; The potential intersection point is:
     (def i1 (pt+ p1 (vec* t v1)))
     (if (or (and (<= 0 s 1) (<= 0 t 1)) 
             (and (<= 0 s 1) (< t 0) a1) 
             (and (<= 0 s 1) (> t 1) b1)

             (and (< s 0) a2 (<= 0 t 1))
             (and (< s 0) a2 (< t 0) a1)
             (and (< s 0) a2 (> t 1) b1)

             (and (> s 1) b2 (<= 0 t 1))
             (and (> s 1) b2 (< t 0) a1)
             (and (> s 1) b2 (> t 1) b1))
         i1
         #f)]))

(define (uniqify-pts ps)
  (let loop ([us (sort (filter values ps) pt<)])
    (match us
      [(list* u u us)          (loop (cons u us))]
      [(list* u u1 us) (cons u (loop (cons u1 us)))]
      [(list u)        us]
      [(list)          '()]
      [_ (error 'uniqify-pts (~a "got: " ps))])))

(define (draw-line l)
  (defm (line from to a b) l)
  (def win (curve-pict-window))
  (defv (lr ur ul ll) (window-corners win))
  ; the four sides of the window
  (def right (new-line lr ur #f #f)) ; segments
  (def upper (new-line ur ul #f #f))
  (def left  (new-line ul ll #f #f))
  (def lower (new-line ll lr #f #f))
  ; intersections if any
  (def (falsify p) (defm (pt x y) p) (if (infinite? x) #f p))
  (def p (line-intersection l right))
  (def q (line-intersection l upper))
  (def r (line-intersection l left))
  (def s (line-intersection l lower))
  ; cases:
  ; 1) two different boundary intersections
  ;    (connect them)
  ; 2) one boundary intersection
  ;    (connect point inside to boundary)
  ; 3) zero boundary intersections
  ;    (either entirely inside or outside)
  (define (connect u v) (draw (curve u -- v)))
  (match (uniqify-pts (list p q r s))
    [(list u v)  (connect u v)]

    [(list u)
     (cond
       [(inside-window? win from)
        (cond
          [(inside-window? win to)
           ; both inside connect the point farthest away
           (if (< (dist from u) (dist to u))
               (connect to u)
               (connect from u))]
          [else
           ; only from is inside
           (connect from u)])]
       [(inside-window? win to)
        ; from outside, to inside
        (connect u to)]
       [else
        ; both from and to outside => nothing in window
        (draw)])]
    
    [(list)      (cond [(and (inside-window? win from)
                             (inside-window? win to))
                        (connect from to)]
                       [else
                        (draw)])]))

(current-draw-line draw-line)


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
  (vec/ v (len v)))

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
; NOTE: This one treats line1 and line2 as true lines
;       i.e. segments and rays are treated as lines
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



;;;

(define eps-geo 1e-10)

(define (line/quadric-intersection A B a b c d e f g)
  ; intersection points between the line AB and the quadric
  ;  a x^2 + b xy + c y^2 + d x + f y + g =0  
  (match-define (pt Ax Ay) A)
  (match-define (pt Bx By) B)
  (define Δy  (- By Ay))
  (define Δx  (- Ax Bx))
  (define Δ   (- (* Ay Bx) (* Ax By)))
  (cond
    [(> (abs Δy) eps-geo)     
     (define aa (+ (* Δy c) (* a (/ (sqr Δx) Δy)) (- (* b Δx))))
     (define bb (+ (* Δy f) (* -1 Δx d) (* 2 a Δx (/ Δ Δy)) (* -1 b Δ)))
     (define cc (+ (* Δy g) (* -1 Δ  d) (* a (/ (sqr Δ) Δy))))
     (for/list ([y (quadratic-solutions* aa bb cc)])
       (pt (+ (* -1 Δx (/ y Δy)) (* -1 (/ Δ Δy))) y))]
    [else
     ; horizontal line (because Δy~0)
     ; a x^2 + b xy + c y^2 + d x + f y + g = ?
     ; Δx a x^2 + Δx b y x + Δx d x = Δx ? - (Δx c y^2 +  + Δx f y + Δx g)
     ; 
     (define aa (* a Δx))
     (define bb (- (* d Δx) (* b Δ)))
     (define cc (+ (* g Δx) (* -1 Δ f) (* c (/ (sqr Δ) Δx))))
     (for/list ([x (quadratic-solutions* aa bb cc)])
       (pt x (/ (- Δ) Δx)))]))


(module+ test
  (check-equal? (line/quadric-intersection (pt 0 1) (pt 1 1) 1 0 0 0 0 -1 0) 
                (list (pt 1 1) (pt -1 1)))
  (check-equal? (line/quadric-intersection (pt 0 1) (pt 1 1) 1 0 1 0 0 -1 0)
                (list (pt 0 1)))
  (check-equal? (line/quadric-intersection (pt 0 1) (pt 1 1) 1 0 1 1 0 -1 0)
                (list (pt 0 1) (pt -1 1)))
  (check-equal? (line/quadric-intersection (pt 0 1) (pt 1 1) 1 0 1 1 1 -1 0)
                (list (pt 0 1) (pt -1 1)))
  (check-equal? (line/quadric-intersection (pt 0 1) (pt 1 1) -1 0 1 1 1 -1 0)
                (list (pt 0 1) (pt 1 1)))
  (check-equal? (line/quadric-intersection (pt 0 1) (pt 1 1) -1 0 -1 1 1 -1 0)
                (list))
  (check-equal? (line/quadric-intersection (pt 0 1) (pt 1 2) 1 0 -1 1 1 -1 0)
                (list (pt -1 0)))
  (check-equal? (line/quadric-intersection (pt 0 1) (pt 1 2) 1 0 1 1 1 -1 0)
                (list (pt -1 0) (pt 0 1)))
  (check-equal? (line/quadric-intersection (pt 0 1) (pt 1 2) 1 0 1 1 1 1 0)
                (list (pt -1 0))))

#;(define (quadric/quadric-intersection bqe1 bqe2)
  (error 'TODO)
  (define realEpsilon 'todo)
  (define e (* 100 (sqrt realEpsilon)))
  (cond
    [(or (> (abs (- a0 b0)) e) (> (abs (- a1 b1)) e) (> (abs (- a2 b2)) e))
     (define c (list (+ (* -2 a0 a2 b0 b2)
                        (*    a0 a2 b1 b1)
                        (* -1 a0 a1 b2 b1)
                        (*    a1 a1 b0 b2)
                        (* -1 a2 a1 b0 b1)
                        (*    a0 a0 b2 b2)
                        (*    a2 a2 b0 b0))
                     (+ (* -1 a2 a1 b0 b4)
                        (* -1 a2 a4 b0 b1)
                        (* -1 a1 a3 b2 b1)
                        (*  2 a0 a2 b1 b4)
                        (* -1 a0 a1 b2 b4)
                        (*    a1 a1 b2 b3)
                        (*  2 a2 a3 b0 b2)
                        (* -2 a0 a2 b2 b3)
                        (*    a2 a3 b1 b1)
                        (* -1 a2 a1 b1 b3)
                        (*  2 a1 a4 b0 b2)
                        (*  2 a2 a2 b0 b3)
                        (* -1 a0 a4 b2 b1)
                        (*  2 a0 a3 b2 b2))
                     (+ (* -1 a3 a4 b2 b1)
                        (*    a2 a5 b1 b1)
                        (* -1 a1 a5 b2 b1)
                        (* -1 a1 a3 b2 b4)
                        (*    a1 a1 b2 b5)
                        (* -2 a2 a3 b2 b3)
                        (*  2 a2 a2 b0 b5)
                        (*  2 a0 a5 b2 b2)
                        (*    a3 a3 b2 b2)
                        (* -2 a2 a5 b0 b2)
                        (*  2 a1 a4 b2 b3)
                        (* -1 a2 a4 b1 b3)
                        (* -2 a0 a2 b2 b5)
                        (*    a2 a2 b3 b3)
                        (*  2 a2 a3 b1 b4)
                        (* -1 a2 a4 b0 b4)
                        (*    a4 a4 b0 b2)
                        (* -1 a1 a1 b3 b4)
                        (* -1 a2 a1 b1 b5)
                        (* -1 a0 a4 b2 b4)
                        (*    a0 a2 b4 b4))
                     (+ (* -1 a4 a5 b2 b1)
                        (*    a2 a3 b4 b4)
                        (*  2 a3 a5 b2 b2)
                        (* -1 a2 a1 b4 b5)
                        (* -1 a2 a4 b3 b4)
                        (*  2 a2 a2 b3 b5)
                        (* -2 a2 a3 b2 b5)
                        (* -1 a3 a4 b2 b4)
                        (* -2 a2 a5 b2 b3)
                        (* -1 a2 a4 b1 b5)
                        (*  2 a1 a4 b2 b5)
                        (* -1 a1 a5 b2 b4)
                        (*    a4 a4 b2 b3)
                        (*  2 a2 a5 b1 b4))
                     (+ (* -2 a2 a5 b2 b5)
                        (*    a4 a4 b2 b5)
                        (*    a5 a5 b2 b2)
                        (* -1 a4 a5 b2 b4)
                        (*    a2 a5 b4 b4)
                        (*    a2 a2 b5 b5)
                        (* -1 a2 a4 b4 b5))))
     (match-define (list c0 c1 c2 c3 c4) c)     
     (define x (read-quartic-roots c0 c1 c2 c3 c4))]
    [(> (abs (- b4 a4)) e)
     (define D (sqr (- b4 a4)))
     (define c (list (/ (+ (* a0 b4 b4)
                           (* (+ (* -1 a1 b3) (* -2 a0 a4) (* a1 a3)) b4)
                           (* a2 b3 b3)
                           (* (+ (* a1 a4) (* -2 a2 a3)) b3)
                           (*    a0 a4 a4)
                           (* -1 a1 a3 a4)
                           (*    a2 a3 a3))
                        D)
                     (/ (- (+ (* (+ (* a1 b4) (* -2 a2 b3) (* -1 a1 a4) (* 2 a2 a3)) b5)
                              (* a3 b4 b4)   
                              (* (+ (*   a4 b3) (* -1 a1 a5) (* a3 a4)) b4)
                              (* (+ (* 2 a2 a5) (* -1 a4 a4)) b3)
                              (* (+ (*   a1 a4) (* -2 a2 a3)) a5)))
                        D)
                     (+ (/ (* a2 (sqr (- a5 b5))) D)
                        (/ (* a4 (- a5 b5)) (- b4 a4))
                        a5)))
     (match-define (list c0 c1 c2) c)
     (define x (quadratic-roots c0 c1 c2))]
    [(> (abs (- a3 b3)) e)
     (define D (- b3 a3))
     (define c (list a2
                     (/ (+ (* -1 a1 b5) (* a4 b3) (* a1 a5) (* -1 a3 a4)) D)
                     (+ (/ (* a0 (sqr (- a5 b5))) (sqr D))
                        (/ (* a3 (- a5 b5)) D)
                        a5)))
     (define ys (quadratic-roots c0 c1 c2))
     (for ([i (in-range 0 (length ys))])
       (define yi (list-ref y i))
       (define cs (list a0 
                        (+ (* a1 yi) a3)
                        (+ (* a2 (sqr yi)) (* a4 yi) a5)))
       (define xs (quadratic-roots c0 c1 c2))
       (for ([j (in-range 0 (length xs))])
         (if (< (abs (+ (* b0 (sqr xj)) (* b1 xj yi) (* b2 (sqr yi)) (* b3 xj) (* b4 yi) b5))
                1e-5)
             (P.push (pt xj yi)))))]
    [(< (abs (- a5 b5)) e)
     (error 'intersectionpoints: "intersection of identical conics")]
    
     
     
     ;;; TODO
;        for (int i = 0; i < y.length; ++i) {
;          c = new real[] {a[0], a[1]*y[i]+a[3], a[2]*y[i]^2 + a[4]*y[i]+a[5]};
;          x = quadraticroots(c[0], c[1], c[2]);
;          for (int j = 0; j < x.length; ++j) {
;            if(abs(b[0]*x[j]^2 + b[1]*x[j]*y[i]+b[2]*y[i]^2 + b[3]*x[j]+b[4]*y[i]+b[5]) < 1e-5)
;              P.push(point(R, (x[j], y[i])));
;          }
;        }
;        return P;
;      } else {
;        if(abs(a[5]-b[5]) < e) abort("intersectionpoints: intersection of identical conics.");
;      }
;    }
;  }
;  for (int i = 0; i < x.length; ++i) {
;    c = new real[] {a[2], a[1]*x[i]+a[4], a[0]*x[i]^2 + a[3]*x[i]+a[5]};
;    y = quadraticroots(c[0], c[1], c[2]);
;    for (int j = 0; j < y.length; ++j) {
;      if(abs(b[0]*x[i]^2 + b[1]*x[i]*y[j]+b[2]*y[j]^2 + b[3]*x[i]+b[4]*y[j]+b[5]) < 1e-5)
;        P.push(point(R, (x[i], y[j])));
;    }
;  }
;  return P;
;}
     (error 'TODO)))

;;; CIRCLE

(struct circle: (c r l)
  #:transparent
  #:property prop:drawable
  (λ (c) ((current-draw-circle) c)))
; c = center
; r = radius
; l = line used if r=+inf.0

(define (draw-circle c)
  (defm (circle: p r l) c)
  (draw (circle p r)))

(current-draw-circle draw-circle)


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

;;;
;;; PARAMETERIZATION
;;;

(struct parameterization (ac a b bc closed? f)
  #:property prop:drawable (λ (p) ((current-draw-parameterization) p))
  #:transparent)
; represents a function from the interval from a to b to pt
;  Interval    ac bc
;   ]a;b[      #f #f
;   ]a;b]      #f #t
;   [a;b[      #t #f
;   [a;b]      #f #t

(define (parameterization->curve p #:steps [n 40])
  (defm (parameterization ac a b bc closed? f) p)
  (def w (- b a))
  (def Δ (/ w n))
  (def ε (min 0.00001 Δ))
  (def a0 (if ac a (+ a ε)))
  (def b0 (if bc b (- b ε)))
  (def pts (append (list (f a0))
                   (for/list ([x (in-range (+ a Δ) b Δ)])
                     (f x))
                   (list (f b0))))
  (curve* (add-between pts ..)))

(define (draw-parameterization p)
  (draw (parameterization->curve p)))

(current-draw-parameterization draw-parameterization)

;;;
;;; PARABOLAS
;;;

(struct parabola (f v)
  #:transparent
  #:property prop:drawable
  (λ (p) ((current-draw-parabola) p)))
; f and v are points:
;   f is the focus 
;   v is the vertex 
; The parabola consists of all points whose distances to f and v are the same

; The focal parameter a is the distance from the from the vertex to the focus.

(define (parabola-focal p)
  (match-define (parabola f v) p)
  (dist f v))

(define (directrix p)
  (match-define (parabola f v) p)
  (define fv (pt- v f))
  (define P  (pt+ v fv))
  (define Q  (pt+ P (rot90 fv)))
  (new-line P Q))

(define (excentricity x)
  (match x
    [(parabola f v) 1]
    ; TODO case for hyperbola
    ; TODO case for ellipse
    [_ (error)]))


(define (parabola-parameterization conic)
  (match conic
    [(and (parabola F V) p)
     (def e (excentricity p))
     (def d (directrix p))
     ; If the focus F is at origo O and D=distance from focus to directrix
     ; then
     ;                 e D
     ;        r = -------------
     ;             1 -e cos(θ)
     ; where θ is is the angle from the x-axis to the point
     (def D (distance F d))
     (def θ0 (angle2 (vec 1 0) (line-normal-vector d)))
     (def eD (* e D))
     (parameterization #f 0 2π #f ; ]0;2π[
                       #f         ; not closed
                       (λ (θ)
                         (let ([θ (- θ θ0)])
                           (def r (/ eD
                                     (- 1 (* e (cos θ)))))
                           (shifted F (pt@ r (+ θ θ0))))))]
     [_ (error 'polar-parameterization
              "TODO: implement the hypoerbola and ellipse cases")]))

(define (draw-parabola p)
  (draw (parabola-parameterization p)))

(current-draw-parabola draw-parabola)
  
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

;;; DRAWING FUNCTIONS

; mark-angle : pt pt pt int number number -> pict
; Mark the directed angle AOB with n arcs that are sep units apart.
; The initial arc has radius r.
(define (mark-angle A O B [n 1] [r 1] [sep 0.5])
  ;TODO: allow mark-angle to work with oriented lines as input
  (define α (angle (pt- A O)))
  (define β (angle (pt- B O)))
  (for/draw ([n (in-range 1 (add1 n))])
    (shifted O (arc (+ r (* sep (sub1 n))) α β))))


