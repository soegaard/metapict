#lang racket
(require "../metapict.rkt" "../polygons.rkt") 

;;;
;;; Figures from:
;;; http://tex.loria.fr/prod-graph/zoonekynd/metapost/metapost.html
;;;

; (draw todo-red ...) will draw a red square below a fig.
; use this to mark figures whose output differes from the
; original metapost figures.

(def todo-red    (color "red"    (fill (square #:side 2))))
(def todo-yellow (color "yellow" (fill (square #:side 2))))



(def fig1 ; straight lines
  (let ([A (pt 0 0)] [B (pt 1 0)] [C (pt 0 1)])
    (draw (curve A -- B -- C))))
(def fig2 ; closed curve with straight lines
  (let ([A (pt 0 0)] [B (pt 1 0)] [C (pt 0 1)])
    (draw (curve A -- B -- C -- cycle))))
(def fig3 ; 3 curves same drawing
  (let ([A (pt -1 -1)] [B (pt 1 -1)] [C (pt 1 1)] [D (pt -1 1)])
    (draw (curve A -- B -- C -- D -- cycle)
          (curve A -- C)
          (curve B -- D))))
(def fig4 ; line thickness
  (let ([A (pt 0 0)])
    (penscale 4 (draw (curve (pt 0 0))))))  ; todo : get rid of draw ?
(def fig5
  (let ([A (pt 0 0)] [B (pt 1 0)] [C (pt 0 1)])
    (draw (curve A -- B -- C -- cycle)
          (penscale 4 (draw A B C)))))
(def fig6
  (let ([A (pt -1 -1)] [B (pt 1 -1)] [C (pt 1 1)] [D (pt -1 1)])
    (draw (curve A -- B -- C -- D -- cycle)
          (curve A -- C)
          (curve B -- D)
          (penscale 4 (draw A B C D)))))
(def fig7 ; mediation aka interpolation between points
  (let ([A (pt 0 0)] [B (pt 1 0)] [C (pt 0 1)])
    (draw (curve A -- B -- C -- cycle)
          (curve (med 1/2 A B) -- C)
          (curve (med 1/2 B C) -- A)
          (curve (med 1/2 C A) -- B))))
(def fig8
  (let ([A (pt 0 0)] [B (pt 1 0)] [C (pt 0 1)])
    (draw (curve A -- B -- C -- cycle)
          (curve (med 1/2 A B) -- C)
          (curve (med 1/2 B C) -- A)
          (curve (med 1/2 C A) -- B)
          (penscale 4 (draw (pt+ (pt* 1/3 A) (pt* 1/3 B) (pt* 1/3 C)))))))
(def fig9
  (let ([A (pt 0 0)] [B (pt 1 0)] [C (pt 0 1)])
    (draw (curve A -- B -- C -- cycle)
          (penscale 2 (draw (curve A -- B))))))
(def fig10
  (let ([A (pt 0 0)] [B (pt 1 0)] [C (pt 0 1)])
    (draw (curve A -- B -- C -- cycle)
          (pencolor (color+ "green" "red") (draw (curve A -- B))))))
(def fig11
  (let ([A (pt 0 0)] [B (pt 1 0)] [C (pt 0 1)])
    (draw (curve A -- B )
          (dashed (draw (curve B -- C)))
          (dotted (draw (curve C -- A))))))
(def fig12 ; colors
  (let ([A (pt 0 0)] [B (pt 1 0)] [C (pt 0 1)])
    (penscale 2 (draw
                 (color .8 "white" (draw (curve A -- B)))
                 (color .6 "white" (draw (curve B -- C)) )
                 (color .4 "white" (draw (curve C -- A)))))))
(def fig13
  (let ([A (pt 0 0)] [B (pt 1 0)] [C (pt 0 1)])
    (draw (curve A -- B -- C -- 'cycle)
          (dashed (draw (curve (med 1/2 A B) -- C)
                        (curve (med 1/2 B C) -- A)
                        (curve (med 1/2 C A) -- B)))
          (color "red" (penscale 4 (draw (pt+ (pt* 1/3 A) (pt* 1/3 B) (pt* 1/3 C))))))))
; fig 14 15 16  dash patterns
(def fig17
  (let ([A (pt 0 0)] [B (pt 1 0)] [C (pt 0 1)])
    (draw (draw-arrow (curve C -- B -- A))
          (penscale 2 (draw-arrow (curve A -- C))))))
(def fig18
  (let ([A (pt 0 0)] [B (pt 1 0)] [C (pt 0 1)])
    (draw (draw (curve C -- B -- A -- cycle))
          (penscale 2 (draw-double-arrow (curve A -- C))))))
(def fig19
  (with-window (window -1.5 1.5 -1.5 1.5)
    (draw (curve (pt -1.5 0) -- (pt 1.5 0))
          (curve (pt 0 -1.5) -- (pt 0 1.5))
          (penscale 2 (draw-arrow (curve (pt 0 0) -- (pt 1 0))))
          (penscale 2 (draw-arrow (curve (pt 0 0) -- (pt 0 1)))))))
(def fig20
  (let ([A (pt 0 0)] [B (pt 1 0)] [C (pt 0 1)])
    (color .8 "white" (fill (curve A -- B -- C -- 'cycle)))))
(def fig21
  (let ([A (pt 0 0)] [B (pt 1 0)] [C (pt 0 1)])
    (draw (color .8 "white" (fill (curve A -- B -- C -- 'cycle)))
          (curve A -- B -- C -- 'cycle))))
(def fig22 ; illustrate border
  (let ([A (pt 0 0)] [B (pt 1 0)] [C (pt 0 1)])
    (draw (color .8 "white" (fill (curve A -- B -- C -- 'cycle)))
          (penscale 2 (draw (curve A -- B -- C -- 'cycle))))))
(def fig23 ; outside border
  (let ([A (pt 0 0)] [B (pt 1 0)] [C (pt 0 1)])
    (draw (penscale 2 (draw (curve A -- B -- C -- 'cycle)))
          (color .8 "white" (fill (curve A -- B -- C -- 'cycle))))))
(def fig24 ; fill of self-intersecting curve 
  (let ([A (pt 0 0)] [B (pt 1 0)] [C (pt 1 1)] [D (pt 0 1)])
    (color .8 "white" (fill (curve A -- C -- B -- D -- 'cycle)))))
(def fig25 ; store curve and use it twice
  (let ([A (pt 0 0)] [B (pt 1 0)] [C (pt 1 1)] [D (pt 0 1)])
    (def c (curve A -- C -- B -- D -- 'cycle))
    (draw (color .8 "white" (fill c))
          c)))
(def fig26
  (with-window (window -1.5 1.5 -1.5 1.5)
    (let ([A (pt -1.5 -1.5)] [B (pt 1.5 -1.5)] [C (pt 1.5 1.5)] [D (pt -1.5 1.5)])
      (draw (color .8 "white" (fill (curve A -- C -- B -- D -- cycle)))
            (curve (pt -1.5 0) -- (pt 1.5 0))
            (curve (pt 0 -1.5) -- (pt 0 1.5))
            (penscale 2 (draw-arrow (curve (pt 0 0) -- (pt 1 0))))
            (penscale 2 (draw-arrow (curve (pt 0 0) -- (pt 0 1))))))))
(def fig27
  (let () (def A (pt 0 0))
    (penscale 4
              (draw (label-top "Above" A)
                    (label-bot "Below" A)
                    (label-rt  "Right" A)
                    (label-lft "Left"  A)))))
(def fig28
  (let () (def A (pt 0 0))
    (penscale 4
              (draw (label-ulft "UpLeft"  A)
                    (label-urt  "UpRight" A)
                    (label-llft "LowLeft"  A)
                    (label-lrt  "LowRight" A)))))
(def fig29
  (let () (def A (pt 0 0))
    (dot-label-urt "A" A)))
(def fig30
  (let ([A (pt 0 0)] [B (pt 1 0)] [C (pt 0 1)])
    (with-window (window -2 2 -2 2)
      (draw (curve A -- B -- C -- cycle)
            (dot-label-llft "A" A)
            (dot-label-lrt  "B" B)
            (dot-label-top  "C" C)))))
(def fig31
  (let ([A (pt 0 0)] [B (pt 1 0)] [C (pt 0 1)])
    (with-window (window -2 2 -2 2)
      (draw (curve A -- B -- C -- cycle)
            (label-bot "1" (med 1/2 A B))
            (label-lft "1" (med 1/2 A C))
            (label-urt "sqrt 2" (med 1/2 B C))))))
(def fig32
  (let ()
    (defv (u -u) (values 0.8 -0.8))
    (defv (A B C D E F) (values (pt -u u) (pt 0 u) (pt u u) (pt -u 0) (pt 0 0) (pt u 0)))
    (draw (curve A -- D) (curve A -- E) (curve A -- F)
          (curve B -- D) (curve B -- E) (curve B -- F)
          (curve C -- D) (curve C -- E) (curve C -- F)
          (dot-label-top "a" A)
          (dot-label-top "b" B)
          (dot-label-top "c" C)
          (dot-label-bot "a'" D)
          (dot-label-bot "b'" E)
          (dot-label-bot "c'" F))))
(def fig33 ; fullcircle
  (draw fullcircle))
(def fig34 ; dot in circle
  (draw (penscale 4 (draw (pt 0 0)))
        fullcircle))
(def fig35 
  (with-window (window -1 3 -1 3)
    (draw (penscale 4 (draw (pt 0 0)))
          (shifted 1 1 (scaled 1 fullcircle)))))
(def fig36 
  (let ([A (pt 0 0)] [B (pt 1 0)] [C (pt 0 1)])
    (with-window (window -.5 1.5 -.5 1.5)
      (draw (curve A -- B -- C -- 'cycle)
            (color "white" (fill (shifted A (scaled (px 4) fullcircle))))
            (color "white" (fill (shifted B (scaled (px 4) fullcircle))))
            (color "white" (fill (shifted C (scaled (px 4) fullcircle))))
            (shifted A (scaled (px 4) fullcircle))
            (shifted B (scaled (px 4) fullcircle))
            (shifted C (scaled (px 4) fullcircle))))))

(def fig37
  (with-window (window -0.6 1.6 -0.6 1.6)
    (draw (curve (pt 0 0) .. (pt 0 1) .. (pt 1 0) .. (pt 1 1))
          (penscale 4 (draw (pt 0 0)))
          (penscale 4 (draw (pt 0 1)))
          (penscale 4 (draw (pt 1 0)))
          (penscale 4 (draw (pt 1 1))))))
(def fig38 
  (with-window (window -1 2 -1 2)
    (draw (curve (pt 0 0) -- (pt 0 1) .. (pt 1 0) .. (pt 1 1)))))
(def fig39 
  (with-window (window -2 2 -2 2)
    (draw (curve (pt 0 0) --- (pt 0 1) .. (pt 1 0) .. (pt 1 1)))))
(def fig40 
  (with-window (window -1 2 -1 2)
    (draw (curve (pt 0 0) .. (pt 0 1) .. (pt 1 0) .. (pt 1 1) .. 'cycle))))
(def fig41 
  (let ([A (pt 0 0)] [B (pt 1 0)] [C (pt 1 1)] [D (pt 0 1)])
    (with-window (window -1 2 -1 2)
      (draw (curve A .. B .. C .. D .. cycle)))))
(def fig42
  (with-window (window -1 2 -1 2)
    (draw (curve (pt 0 0) .. (pt 1 1) .. cycle))))
(def fig43
  (with-window (window 0 2 -1 1)
    (draw (curve (pt 0 0) (vec 0 1) .. (vec 1 0) (pt 2 0)))))
(def fig44 
  (with-window (window 0 2 -1 1)
    (draw (curve (pt 0 0) (dir 90) .. (dir 0) (pt 2 0)))))
(def fig45 
  (with-window (window 0 2 -1 1)
    (draw (curve (pt 0 0) up .. right (pt 2 0)))))
(def fig46 
  (with-window (window 0 2 -1 1)
    (draw (curve (pt 0 0) up .. up (pt 2 0)))))
(def fig47 ; curl
  (with-window (window -1.2 2.2 -1.2 2.2)
    (draw (curve (pt 0 0) up .. (pt 2 0) up .. cycle))))
(define (unit θ) ; return point on unit circle, θ in degrees
  (pt (cos (rad θ)) (sin (rad θ))))
(def fig48 
  (with-window (window 0 2 -1 1)
    (def α 30)
    (draw (curve (pt 0 0) -- (pt* 2 (unit 0)))
          (curve (pt 0 0) -- (pt* 2 (unit α)))
          (curve (unit 0) (dir 90) .. (dir (+ 90 α)) (unit α)))))
(def fig49
  (with-window (window 0 2 -1 1)
    (def α 30)
    (draw (curve (pt 0 0) -- (pt* 2 (unit 0)))
          (curve (pt 0 0) -- (pt* 2 (unit α)))
          (curve (unit 0) (dir 90) .. (dir (+ 90 α)) (unit α))
          (curve (pt* 1.1 (unit 0)) (dir 90) .. (dir (+ 90 α)) (pt* 1.1 (unit α))))))

(define (unitvec v) (vec* (/ (norm v)) v))

(def fig50
  (let ()
    (define (draw-angle A O B n)
      (define (angle d)
        (curve (pt+ O (vec* d (unitvec (pt- A O)))) 
               (rotatedd 90 (vec* d (unitvec (pt- A O))))
               ..
               (rotatedd 90 (vec* d (unitvec (pt- B O))))
               (pt+ O (vec* d (unitvec (pt- B O))))))
      (apply draw (for/list ([r (list 5 4.5 4 5.5)] [_ n]) 
                    (angle (* 2 (px r))))))
    (with-window (window 0 3 0 3)
      (let ([A (pt 0 0)] [B (pt 3 0)] [C (pt 1 2)])
        (draw (curve A -- B -- C -- cycle)
              (draw-angle B A C 1)
              (draw-angle C B A 2)
              (draw-angle A C B 3))))))
(def fig51
  (let ()
    (def u 1) 
    (def p (curve (pt u 0) up .. down (pt (- u) 0) -- cycle))
    (draw (color .8 "white" (fill p))
          (penwidth 1 (draw p)))))
(def fig52
  (let ()
    (define (u x) (* 1 x))
    (def p (curve (pt (u  1) 0) up .. down (pt (u -1) 0)  
                  -- (pt (u -.2) 0) up .. down (pt (u  .2) 0) -- cycle))
    (draw (color .8 "white" (fill p))
          (penwidth 1 (draw p)))))
(def fig53
  (let ()
    (define (u x y) (pt (* 1 x) (* 1 y)))
    (let ([A (u .2 .05)] [AA (u .2 -.05)] [B (u 1 .05)] [BB (u 1 -.05)]
                         [C  (u -.2 0)] [D (u -1 0)])
      (def p (curve B up .. D down .. up BB  -- AA down .. C up .. down A  -- cycle))
      (draw (color .8 "white" (fill p))
            (penwidth 1 (draw p))))))
(def fig54
  (let ([A (pt 0 0)] [B (unit 60)] [C (unit -60)] [D (pt 2 0)])
    (with-window (window -0.1 4.1 -2.1 2.1)
      (draw (penscale 4 (draw A))
            (penscale 4 (draw B))
            (penscale 4 (draw C))
            (penscale 4 (draw D))
            (curve A -- D -- B -- D -- C)
            (curve A up .. B)
            (curve A down .. C)
            (curve A (dir 30) .. B)
            (curve A (dir -30) .. C)))))
(def fig55 
  (with-window (window -1 3 -1 3)
    (draw (curve (pt 0 0) .. (pt 1 1) .. (pt 2 0)))))
(def fig56 
  (with-window (window 0 3 0 3)
    (draw (curve (pt 0 0) (tension 2) (pt 1 1) .. (pt 2 0)))))
(def fig57 
  (with-scaled-window 1.1
    (draw (curve (pt 0 1)    right      .. 
                 (unit -150) (dir  120) .. 
                 (unit -30)  (dir -120) .. cycle))))
(def fig58
  (draw (curve (pt 0 1)    right      (tension 2)
               (unit -150) (dir  120) (tension 2)
               (unit -30)  (dir -120) (tension 2) cycle)))
(def fig59
  (draw (curve (pt 0 1)    right      (tension 4)
               (unit -150) (dir  120) (tension 4)
               (unit -30)  (dir -120) (tension 4) cycle)))
(def fig60
  (let ()
    (def u 0.25)
    (def A (pt 0 0))
    (def B (pt (- u) (* 2 u)))
    (def C (pt (* 4 u) (* 3 u)))
    (def D (pt (* 3 u) 0))
    (draw (curve A (controls-and B C) D)
          (curve B -- C)
          (draw-arrow (curve A -- B))
          (draw-arrow (curve D -- C)))))

(def fig61
  (let ()
    (def u 0.25)
    (def A (pt     0       0))
    (def D (pt (*  2 u)    0))
    (def B (pt (* -1 u) (* 2 u)))
    (def C (pt (*  3 u) (* 3 u)))
    
    (def a1 A)
    (def a2 B)
    (def a3 C)
    (def a4 D)
    
    (def a12   (med 1/2 a1   a2))
    (def a23   (med 1/2 a2   a3))
    (def a34   (med 1/2 a3   a4))
    (def a123  (med 1/2 a12  a23))
    (def a234  (med 1/2 a23  a34))
    (def a1234 (med 1/2 a123 a234))

    (draw (penscale 4 (draw A B C D))
          (curve A (controls B C) D)
          (penscale 4 (draw a1234)))))

(def fig63 ; illustrates a direction specifier after the last point
  (with-window (window -3.1 3.1 -3.1 3.1)
    (draw (curve (pt 0 0) up .. (pt 1 1/10) .. (pt 2 0) down))))

; figure 64
;  draw (0,0){up} .. (1cm, 1mm) .. (2cm,0){down};
;       p0=(0,0) {up (tension-and 1 1) #f} (pt 1 1/10) {#f (tension 1 1) #f} (pt 2 0) down)
;                

(def fig64 ; ... todo: has negative tension,  not supported yet
    (with-window (window -.1 2.1 -.1 2.1)
      (draw todo-red
            (curve (pt 0 0) up ... (pt 1 1/10) ... (pt 2 0) down))))
(def fig65
  (with-scaled-window 2
    (draw (curve (pt 0 0) (curl 0) .. (pt 0 1) .. (pt 1 0) .. (pt 1 1)))))
(def fig72
  (let () (def p (scaled 5 fullcircle))
    (define (shift k p) (shifted k (* 2 k) p))
    (with-window (window -5 15 -5 15)
      (draw p 
            (shift 1 p)
            (shift 2 p)
            (shift 3 p)
            (shift 4 p)
            (shift 5 p)))))
(def fig73
  (let () 
    (def p (curve (pt .5  -.5) right .. (pt 2 0)))
    (with-window (window -1 4 -1 4)
      (draw p
            (rotatedd  0 p)
            (rotatedd 10 p)
            (rotatedd 20 p)
            (rotatedd 30 p)
            (rotatedd 40 p)
            (rotatedd 50 p)
            (rotatedd 60 p)
            (rotatedd 70 p)
            (rotatedd 80 p)
            (rotatedd 90 p)))))
(def fig74
  (let () (def p (scaled 5/10 fullcircle))
    (draw p
          (xscaled 2 p)
          (xscaled 3 p)
          (xscaled 4 p))))
(def fig75
  (let ()
    (def A (pt 1 0))
    (def B (rotatedd 72 A))
    (def C (rotatedd 72 B))
    (def D (rotatedd 72 C))
    (def E (rotatedd 72 D))
    (draw (curve A -- B -- C -- D -- E -- cycle))))
(def fig76
  (let ()
    (def A (pt 1 0))
    (def B (rotatedd 72 A))
    (def C (rotatedd 72 B))
    (def D (rotatedd 72 C))
    (def E (rotatedd 72 D))
    (draw (curve A -- C -- E -- B -- D -- cycle))))
(def fig78
  (let ()
    (with-window (window -0.5 2.5 -0.5 2.5)
      (def c (curve (pt 0 0) -- (pt 1 0)))
      (draw (penscale 2 (draw-arrow c))
            (draw-arrow (zscaled 1 2 c))))))
#;(def fig95 ; todo: dir after last point (handle in path: in path.rkt)
  (let* ([A (pt 0 1)] [B (rotatedd 120 A)])
    (def p (curve A (dir 0) (tension 2) B (dir 120)))
    (draw p 
          (rotatedd  120 p)
          (rotatedd -120 p))))
#;(def fig96 ; todo: dir after last point (handle in path: in path.rkt)
  (let () 
    (def n 5)
    (def 360/n (/ 360 n))
    (define (rot m p) (rotatedd (* m 360/n) p))
    (def A (pt 0 1))
    (def B (rot 2 A))
    (def p (curve A (dir 180) (tension 4) B (dir (+ 180 (* 2 360/n)))))
    (draw (color "red" (draw p))
          (rot 1 p)
          (rot 2 p)
          (rot 3 p)
          (rot 4 p))))
#;(def fig97 ; TODO: dir after last point
  (let ()
    (def c0 (shifted 0 1 (yscaled .5 (xscaled 1 fullcircle))))
    (def c1 (rotatedd 120 c0))
    (def c2 (rotatedd 120 c1))
    (def A (pt -.5 1))
    (def l0 (curve A down .. 
                   (rotatedd 120 (xscaled -1 A)) 
                   (rotatedd 120 (vec- (vec 0 0) down))))
    (def l1 (rotatedd 120 l0))
    (def l2 (rotatedd 120 l1))
    (with-scaled-window 2
      (draw c0 c1 c2 l0 l1 l2))))
#;(def fig98 ; todo dir after last point
  (let ()
    (def c0 (shifted 0 1 (yscaled .5 (xscaled 1 fullcircle))))
    (def c1 (rotatedd 360/4 c0))
    (def c2 (rotatedd 360/4 c1))
    (def c3 (rotatedd 360/4 c2))
    (def A (pt -.5 1))
    (def l0 (curve A down .. 
                   (rotatedd 360/4 (xscaled -1 A))
                   (rotatedd 360/4 (vec- (vec 0 0) down))))
    (def l1 (rotatedd 360/4 l0))
    (def l2 (rotatedd 360/4 l1))
    (def l3 (rotatedd 360/4 l2))
    (with-scaled-window 2
      (draw c0 c1 c2 c3 l0 l1 l2 l3))))
(def fig99
  (let ()
    (def A (pt 0 0)) (def B (pt 2 1))
    (with-scaled-window 2
      (draw (penscale 4 (draw A))
            (penscale 4 (draw B))
            (shifted (med 1/2 A B)
                     (scaled (norm (pt- B A))
                             fullcircle))))))
(def fig102
  (let () (def A (pt 0 0)) (def B (pt 3 0)) (def C (pt 1 2))
    (define (num x) (~r x #:precision 2))
    (with-scaled-window 4
      (draw (curve A -- B -- C -- cycle)
            (label-llft (num (deg (- (angle (pt- C A)) (angle (pt- B A))))) A)
            (label-lrt  (num (deg (- (angle (pt- A B)) (angle (pt- C B))))) B)
            (label-urt  (num (deg (- (angle (pt- B C)) (angle (pt- A C))))) C)))))
#;(def fig105 ; todo dir after last point
  (let () (def p (curve (pt 0 0) up .. (pt 2 0) down))
    (with-scaled-window 2
      (draw p
            (point-of p 0)
            (point-of p 1/2)
            (point-of p 1)))))
#;(begin
(def fig113
  (let ()
    (def p (curve (pt 0 0) up .. (pt 2 0) up))
    (def q (curve (pt 0 1) (dir -60) .. (pt 1 -1) .. (dir 60) (pt 2 1)))
    (with-scaled-window 2
      (draw p q (penscale 4 (draw (intersection-point p q)))))))
(def fig114
  (let ()
    (def p (curve (pt 0 0) up .. (pt 2 0) up))
    (def q (curve (pt 0 1) (dir -60) .. (pt 1 -1) .. (dir 60) (pt 2 1)))
    (with-scaled-window 2
      (defv (a _) (intersection-times p q))
      (draw p q (penscale 4 (draw (point-of p a)))))))
(def fig115
  (let () 
    (def rot120 (rotatedd 120))
    (def rot-120 (rotatedd -120))
    (def A (pt 0 1))
    (def B (rot120 A))
    (def p (curve A (dir 0) (tension 2) B (dir 120)))
    (defv (a _) (intersection-times p (rot120 p)))
    (draw (subcurve p 0 (- a .02))
          (subcurve p (+ a .02) 1)
          (rot120 (subcurve p 0 (- a .02)))
          (rot120 (subcurve p (+ a .02) 1))
          (rot-120 (subcurve p 0 (- a .02)))
          (rot-120 (subcurve p (+ a .02) 1)))))
(def fig156
  (let ()
    (define (random-pt) (pt (- (random) .5) (- (random) .5)))
    (define (random-path n)
      (curve* (cons (random-pt)
                    (for/list ([i n])
                      (list .. (random-pt))))))
    (define (intersections p q)
      (def n 10)
      (def lenp (curve-length p))
      (def lenq (curve-length q))
      (penscale 
       4 (draw* (for*/list ([i (in-range 0 lenp (/ lenp n))]
                            [j (in-range 0 lenq (/ lenq n))])
                  (defv (a b) (intersection-times
                               (subcurve p i (+ i (/ lenp n)))
                               (subcurve q j (+ j (/ lenq n)))))
                  (and a (point-of (subcurve p i (+ i (/ lenp n))) a))))))
    (def p (random-path 4))
    (def q (random-path 4))
    (draw (color "red" (draw p))
          (color "blue" (draw q))
          (intersections p q))))
(def fig164
  (with-window (window -3 3 -3 3)
    (let ()
      (def C (pt+ origo (vec* 3/10 up))) ; 3mm = 3/10
      (def A (rotatedd  120 C))
      (def B (rotatedd -120 C))
      (def pic (draw (curve A -- B -- C -- cycle)
                     (penscale 4 (draw A B C))))
      (def unclipped
        (draw*
         (for*/list ([i (in-range -3 4)] [j (in-range -3 4)])
           (def pos (pt+ (pt 0 0) (vec+ (vec* i (pt- B A)) 
                                        (vec* j (pt- C A)))))
           (label-cnt pic pos))))
      (def region (curve (pt -2 -2) -- (pt 2 -2) -- (pt 2 2) -- (pt -2 2) -- cycle))
      (draw (color "blue" unclipped)
            (color "green" (clipped unclipped region))
            (color "red" (draw region))))))
)
(list "Figure   1-  5"  fig1  fig2    fig3   fig4   fig5)
(list "Figure   6- 10"  fig6  fig7    fig8   fig9   fig10)
(list "Figure  11- 15"  fig11 fig12   fig13     14     15)
(list "Figure  16- 20"     16 fig17   fig18  fig19  fig20)
(list "Figure  21- 25"  fig21 fig22   fig23  fig24  fig25)
(list "Figure  26- 30"  fig26 fig27   fig28  fig29  fig30)
(list "Figure  31- 35"  fig31 fig32   fig33  fig34  fig35)
(list "Figure  36- 40"  fig36 fig37   fig38  fig39  fig40)
(list "Figure  41- 45"  fig41 fig42   fig43  fig44  fig45)
(list "Figure  46- 50"  fig46 fig47   fig48  fig49  fig50)
(list "Figure  51- 55"  fig51 fig52   fig53  fig54  fig55)
(list "Figure  56- 60"  fig56 fig57   fig58  fig59  fig60)
(list "Figure  61- 65"  fig61    62   fig63  fig64  fig65)
(list "Figure  71- 75"     71 fig72   fig73  fig74  fig75)
(list "Figure  76- 80"  fig76    77   fig78     79     80)
(list "Figure  91- 95"     91    92      93     94  #;fig95)
(list "Figure  96-100"  96 #;fig96 #;fig97   #;fig98  fig99    100)
(list "Figure 101-105"    101 fig102    103    104    105)
#;((list "Figure 111-115"    111    112 fig113 fig114 fig115)
(list "Figure 156-160" fig156    157    158    159    160)
(list "Figure 161-165"    161    162    163 fig164    165))

