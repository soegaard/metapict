#lang racket
(require metapict)


(define infinity (make-parameter 1000))

; unit vector in the direction of v
(define (unit v)
  (vec* (/ 1 (norm v)) v))

; unbounded line through A and B
(define (line* A B)
  (def r  (pt- B A))
  (def ir (vec* (infinity) r))
  (curve (pt+ A ir) .. (pt- A ir)))

; line segment from A to B
(define (line A B)
  (curve A .. B))

(define (ray A B)
  (def AB (pt- B A))
  (curve A .. (pt+ A (vec* (infinity) AB))))

; line through A orthogonal to AB
(define (orthogonal* A B)
  (def AB (pt- B A))
  (line* A (pt+ A (rot90 AB))))

; distance from A to line through BC
(define (dist-pt/line A B C)
  (dist A (intersection-point (line* A (pt+ A (rot90 (pt- C B))))
                              (line* B C))))

(define (midpoint A B)
  (med .5 A B))

(define (projection-point/line A B C)
  ; Calculate the projection of point A on the line l
  ; through A and B.
  (intersection-point (line* A (pt+ A (rot90 (pt- C B))))
                      (line* B C)))


; find the point in the list xs which has the
; greatest distance to A
(define (farthest A Ps)
  (define (d B) (dist A B))
  (define (greater? B C) (> (d B) (d C)))
  (first (sort Ps greater?)))

;;;
;;; Height
;;;


(define (height A B C)
  ; Find height h_a through A onto BC.
  (def BC (pt- C B))
  ; line l through A perpendicular to BC
  (def l (line* A (pt+ A (rot90 BC))))
  ; foot point on (extension of) BC of height through A
  (def foot (intersection-point (line* B C) l))
  (displayln (list A B C foot))
  (curve foot .. A))

;;;
;;; Median
;;;

; median from A to midpoint of BC
(define (median A B C)
  (line A (midpoint B C)))

(define (median* A B C)
  (line* A (midpoint B C)))

;;;
;;; (Angle) Bisector
;;;

(define (bisector* A B C)
  (def AB (pt- B A))
  (def AC (pt- C A))
  (def α (+ (signed-angle AB) (signed-angle AC)))
  (def β (* (sgn (dot AB AC)) α))
  (line* A (pt+ A (vec@ 1 (/ α 2.)))))

(define (bisector A B C)
  (def F (intersection-point (line B C) (bisector* A B C)))
  (line A F))

;;;
;;; Perpendicular Bisector
;;;

(define (perpendicular-bisector* A B C)
  (def M (midpoint B C))
  (def BC (pt- C B))
  (line* M (pt+ M (rot90 BC))))

;;;
;;; Inscribed Circle and Incenter
;;;

(define (incenter A B C)
  (intersection-point (bisector* A B C)
                      (bisector* B A C)))

(define (inradius A B C)
  (dist-pt/line (incenter A B C) B C))

(define (incircle A B C)
  (circle (incenter A B C) (inradius A B C)))

;;;
;;; Circumcircle (Circumscribed circle)
;;;

; The circum center is the center of the circle passing through
; the three vertices of the triangle.

(define (circumcenter A B C)
  (intersection-point (perpendicular-bisector* A B C)
                      (perpendicular-bisector* B A C)))

(define (circumradius A B C)
  (dist A (circumcenter A B C)))

(define (circumcircle A B C)
  (def CC (circumcenter A B C))  
  (circle CC (dist B CC)))


;;;
;;; Examples
;;;


(define (draw-triangle-sides A B C)
  (draw (line A B) (line A C) (line B C)))

(define (draw-triangle-sides* A B C)
  (draw (line* A B) (line* A C) (line* B C)))

(define (draw-triangle-heights A B C)
  (draw (height A B C) (height B C A) (height C A B)))

(define (draw-triangle-bisectors A B C)
  (draw (bisector A B C) (bisector B A C) (bisector C A B)))

(define (draw-triangle-perpendicular-bisectors A B C)
  (draw (perpendicular-bisector* A B C)
        (perpendicular-bisector* B A C)
        (perpendicular-bisector* C A B)))

(define (draw-triangle-medians A B C)
  (draw (median A B C) (median B A C) (median C A B)))

;;;
;;; EXAMPLE
;;;

(define A (pt 0 0))
(define B (pt 5 0))
(define C (pt 7 5))

; HEIGHTS

(set-curve-pict-size 200 200)
(def win (window/aspect -5 10))
(with-window win
  (draw
   ; Draw extensions of side
   (color "gray" (dashed (draw-triangle-sides* A B C)))
   ; Draw sides
   (draw-triangle-sides A B C)
   ; Draw heights
   (color "red" (draw (draw-triangle-heights A B C)))))

; BISECTORS

(with-window win
  (draw
   ; Draw sides
   (draw-triangle-sides A B C)
   ; Draw bisectors
   (color "red" (draw-triangle-bisectors A B C))))

; INCIRCLE

(with-window win
  (draw
   (draw-triangle-sides A B C)
   (incenter A B C)
   (draw-triangle-bisectors A B C)
   ; Circle
   (color "red" (draw (incircle A B C)))))

; MEDIANS

(with-window win
  (draw
   ; Draw sides
   (draw-triangle-sides A B C)
   ; Draw bisectors
   (color "red" (draw-triangle-medians A B C))))

; CIRCUMCIRCLE

(with-window win
  (draw
   (draw-triangle-sides A B C)
   (incenter A B C)
   (color "gray" (dashed (draw-triangle-perpendicular-bisectors A B C)))
   (color "red" (draw (circumcircle A B C)))))

;;;
;;; Constructions
;;;


; Construction of perpendicular bisector

(define (construction:perpendicular-bisector A B)
  (def circle-A (circle A (dist A B)))
  (def circle-B (circle B (dist A B)))
  (def is (intersection-points circle-A circle-B))
  (draw (color "blue" (draw (line A B)))
        circle-A
        circle-B
        (color "red" (draw (line (first is) (second is))))))

(with-window (window/aspect -2.1 2.1)
  (construction:perpendicular-bisector (pt -1 0) (pt 1 0)))

(define (construction:angle-bisector B A C [r 1])
  ; Angle BAC
  (def P (pt+ A (vec* r (unit (pt- B A)))))
  (def Q (pt+ A (vec* r (unit (pt- C A)))))
  (def circle-P (circle P r))
  (def circle-Q (circle Q r))
  (def R (farthest A (intersection-points circle-P circle-Q)))
  (draw (line A B)
        (line A C)
        (circle A r)
        (dot-label "" P)
        (dot-label "" Q)
        circle-P
        circle-Q
        (dot-label "" R)
        (ray A R)))


#;(with-window (window/aspect -2.1 5.1)
    (construction:angle-bisector (pt 3 0) (pt 0 0) (pt 3 4) 2))


(define (construction:triangle-sss a b c)
  (def A (pt 0 0))
  (def B (pt c 0))
  (def α (arccos (/ (+ (sqr b) (sqr c) (* -1 (sqr a)))
                    (* 2 b c))))
  (def C (pt@ b α))
  (draw (line A B)
        (line A C)
        (line B C)))

(with-window (window/aspect -10.1 10.1)
  (construction:triangle-sss 4 8 10))

(define (construction:triangle-ass α b c)
  ; angle α, sides b and c
  (def A (pt 0 0))
  (def B (pt c 0)) ; |AB|=c
  (def C (pt@d b α))
  (draw (line A B)
        (line A C)
        (line B C)))

(with-window (window/aspect -10.1 10.1)
  (construction:triangle-ass 90 3 4))

(define (construction:triangle-sas a β c)
  ; side a, angle β, side c
  (def A (pt 0 0))
  (def B (pt c 0)) ; |AB|=c
  (def C (pt+ B (pt@d a β)))
  (draw (line A B)
        (line A C)
        (line B C)))

(with-window (window/aspect -10.1 10.1)
  (construction:triangle-sas 3 90 4))

(define (construction:triangle-ssa a b γ)
  ; sides a,b angle γ
  (def c (sqrt (+ (sqr a) (sqr b) (* -2 a b (cosd γ)))))
  (def α (arccos (/ (+ (sqr b) (sqr c) (* -1 (sqr a)))
                    (* 2 b c))))
  (def A (pt 0 0))
  (def B (pt c 0)) ; |AB|=c
  (def C (pt@ b α))
  (draw (line A B)
        (line A C)
        (line B C)))

(with-window (window/aspect -10.1 10.1)
  (construction:triangle-ssa 3 4 90))

(define (construction:triangle-aas α β c)
  (def γ (- 180. α β))
  (def b (* (/ c (sind γ)) (sind β)))
  (def A (pt 0 0))
  (def B (pt c 0)) ; |AB|=c
  (def C (pt@d b α))
  (draw (line A B)
        (line A C)
        (line B C)))

(with-window (window/aspect -10.1 10.1)
  (construction:triangle-aas 90 36.87 5))

(define (construction:triangle-asa α b γ)
  (def β (- 180. α γ))
  (def c (* (/ b (sind β)) (sind γ)))
  (def A (pt 0 0))
  (def B (pt c 0)) ; |AB|=c
  (def C (pt@d b α))
  (draw (line A B)
        (line A C)
        (line B C)))

(with-window (window/aspect -10.1 10.1)
  (construction:triangle-asa 90 5 36.87))

(define (construction:triangle-saa a β γ)
  (def α (- 180. β γ))
  (def c (* (/ a (sind α)) (sind γ)))
  (def A (pt 0 0))
  (def B (pt c 0)) ; |AB|=c
  (def C (pt+ B (pt@d a β)))
  (draw (line A B)
        (line A C)
        (line B C)))

(with-window (window/aspect -10.1 10.1)
  (construction:triangle-saa 5 90 36.87))

  

(define (construction:projection-vector-on-vector v w A B)
  ; Show construction of the projection og vector v on w.
  ; The vectors v and w are drawn with start points A and B respectively.
  (def A1 (pt+ A v))
  (def B1 (pt+ B w))
  (def C  (pt+ B w))
  (def v0-proj (projection-point/line A  B C))
  (def v1-proj (projection-point/line A1 B C))
  (draw (dashed (color "gray" (draw (line* B B1)
                                    (curve A  -- v0-proj)
                                    (curve A1 -- v1-proj))))
        (draw-arrow (curve A -- A1))
        (draw-arrow (curve B -- B1))        
        (color "red"  (draw-arrow (curve v0-proj -- v1-proj)))))

(with-window (window/aspect -1.1 8.1)
  (construction:projection-vector-on-vector (vec 4 3) (vec 1 0)
                                            (pt 3 1) (pt 0 0)))




