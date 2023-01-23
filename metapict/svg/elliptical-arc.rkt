#lang racket
(require racket/gui)
; The problem is to draw an elliptical arc from a point (x1,x2) to (x2,y2).
; The major and minor semiaxes are r1 and r2.
; The angle between the x-axis and the major axis is θ.

; The goal is to compute the center (cx,cy) and start angle θ1 and angle size theta Δθ.

(provide elliptical-arc-path)

; Note: Below θ is in radian. In svg the angle is in degrees.
(define (elliptical-arc-path x1 y1 x2 y2 rx ry θ flag-large-arc flag-sweep)
  #;(displayln
   (list 'elliptical-arc-path
         x1 y1 x2 y2 'rx rx 'ry ry 'angle θ 'large flag-large-arc 'sweep flag-sweep))
  ; Make sure radii are positive
  (set! rx (abs rx))
  (set! ry (abs ry))
  ; Make the midpoint C the origin
  (define x1- (+ (*    (cos θ)  (/ (- x1 x2) 2.))  (* (sin θ) (/ (- y1 y2) 2.))))
  (define y1- (+ (* (- (sin θ)) (/ (- x1 x2) 2.))  (* (cos θ) (/ (- y1 y2) 2.))))
  ; Ensure radii are large enough (for the arc to stretch from P to Q)
  (define Λ (+ (/ (sqr x1-) (sqr rx))
               (/ (sqr y1-) (sqr ry))))
  (when (> Λ 1)
    (set! rx (* (sqrt Λ) rx))
    (set! ry (* (sqrt Λ) ry)))
  ; Compute the center in these coordinates
  (define s  (if (= flag-large-arc flag-sweep) -1. 1.))
  (define α  (sqrt (max 0. (/ (+ (* rx rx ry ry) (* -1. rx rx  y1- y1-) (* -1. ry ry x1- x1-))
                           (+ (* rx rx  y1- y1-) (* ry ry x1- x1-))))))
  (define cx- (* s α (/ (*     rx y1-) ry)))
  (define cy- (* s α (/ (* -1. ry x1-) rx)))
  ; Transform back to the center
  (define cx (+ (* (cos θ) cx-)  (* (- (sin θ)) cy-)   (/ (+ x1 x2) 2.)))
  (define cy (+ (* (sin θ) cx-)  (*    (cos θ)  cy-)   (/ (+ y1 y2) 2.)))
  ; (displayln (list 'center cx cy))
  ; Compute the start angle (from the circle before it is stretched and rotated)
  (define (angle x1 y1 x2 y2)
    (define (norm x y) (sqrt (+ (* x x) (* y y))))
    (define (dot x1 y1 x2 y2) (+ (* x1 x2) (* y1 y2)))
    (define s (if (< (- (* x1 y2) (* y1 x2)) 0) -1. +1.))
    (* s (acos (max -1. (min 1. (/ (dot x1 y1 x2 y2)
                                   (* (norm x1 y1) (norm x2 y2))))))))
  (define θ1 (angle 1. 0.  (/ (- x1- cx-) rx) (/ (- y1- cy-) ry)))
  (define Δθ (angle (/ (- x1- cx-) rx)         (/ (- y1- cy-) ry)
                    (/ (- (* -1. x1-) cx-) rx) (/ (- (* -1. y1-) cy-) ry)))
  ; Adjust Δθ 
  ; (displayln (list 'before 'θ1 θ1 'Δθ Δθ))
  (cond
    [(and (= flag-sweep 0) (> Δθ 0)) (set! Δθ (- Δθ (* 2 pi)))]
    [(and (= flag-sweep 1) (< Δθ 0)) (set! Δθ (+ Δθ (* 2 pi)))])
  (define θ2 (if (< (+ θ1 Δθ) 0)
                 (+ θ1 Δθ (* 2 pi))
                 (+ θ1 Δθ)))
  ; (displayln (list 'after 'θ1 θ1 'θ2 θ2 'Δθ Δθ))
  #;(displayln (list θ1 θ2))

  #;(set!-values (θ1 θ2) (if (= flag-sweep 0)
                           (values θ2 θ1)
                           (values θ1 θ2)))
  
  (define dc-path (new dc-path%))
  ; draw elliptical arc with center in (0,0)
  (define ccw? (= flag-sweep 0))
  (send dc-path arc (- rx) (- ry) (* 2 rx) (* 2 ry) (- θ1) (- θ2) ccw?)
  ;(send dc-path arc (- rx) (- ry) (* 2 rx) (* 2 ry) 0 (* 2 pi)  (= flag-sweep 0))
  (send dc-path rotate (- θ))
  (send dc-path translate cx cy)
  dc-path)


#;(begin
  ;; Test: The flags flag-large-arc and flag-sweep matches this drawing
  ;; (check the source of the svg file)
  ;; https://www.w3.org/TR/SVG/images/paths/arcs02.svg
  ;; Given the center (cx,xy) of a non-rotated ellipse with semi-axes rx and ry.
  ;; The upper left corner is (cx-rx,cx-ry) and width=2rx, height=2ry.
  (define x1 125)
  (define y1  75)
  (define x2 100)
  (define y2  50)
  (define rx  100)
  (define ry   50)
  (define θ   (* (/ 0 180) pi))
  (define flag-large-arc 1)
  (define flag-sweep     1)

  (define bm                (make-object bitmap% 400 400))
  (define bm-dc             (new bitmap-dc% [bitmap bm]))
  (define transparent-brush (new brush% [color "black"] [style 'transparent] ))
  (define solid-brush       (new brush% [color "black"] [style 'solid]       ))
  (define pen               (new pen%   [color "black"]  [width 2] [cap 'round] [join 'round]))
  (define green1-pen        (new pen%   [color "green"]  [width 2] [cap 'round] [join 'round]))
  (define red4-pen          (new pen%   [color "red"]    [width 2] [cap 'round] [join 'round]))
  (define blue4-pen         (new pen%   [color "blue"]   [width 2] [cap 'round] [join 'round]))

  (send bm-dc set-pen   pen)
  (send bm-dc set-brush transparent-brush)

  (send bm-dc set-pen   red4-pen)
  (send bm-dc draw-path (elliptical-arc-path x1 y1 x2 y2 rx ry θ 0 0)) ; large-arc, sweep
  (send bm-dc set-pen   blue4-pen)
  (send bm-dc draw-path (elliptical-arc-path x1 y1 x2 y2 rx ry θ 0 1))

  (send bm-dc set-pen   pen)
  (send bm-dc draw-path (elliptical-arc-path x1 y1 x2 y2 rx ry θ 1 0))
  (send bm-dc set-pen   green1-pen)
  (send bm-dc draw-path (elliptical-arc-path x1 y1 x2 y2 rx ry θ 1 1))

  (send bm-dc set-pen    red4-pen)
  (send bm-dc draw-point x1 y1)
  (send bm-dc draw-point x2 y2)

  (send bm-dc set-pen    blue4-pen)
  ;(send bm-dc draw-point cx cy)
  bm
  )

#;(begin
  ;; (define x1 125)
  ;; (define y1  75)
  ;; (define x2 100)
  ;; (define y2  50)
  ;; (define rx  100)
  ;; (define ry   50)
  ;; (define θ   (* (/ 0 180) pi))
  ;; (define flag-large-arc 1)
  ;; (define flag-sweep     1)

  (define bm                (make-object bitmap% 50 50))
  (define bm-dc             (new bitmap-dc% [bitmap bm]))
  (define transparent-brush (new brush% [color "black"] [style 'transparent] ))
  (define solid-brush       (new brush% [color "black"] [style 'solid]       ))
  (define pen               (new pen%   [color "black"]  [width 2] [cap 'round] [join 'round]))
  (define green1-pen        (new pen%   [color "green"]  [width 2] [cap 'round] [join 'round]))
  (define red4-pen          (new pen%   [color "red"]    [width 2] [cap 'round] [join 'round]))
  (define blue4-pen         (new pen%   [color "blue"]   [width 2] [cap 'round] [join 'round]))

  (send bm-dc set-pen   pen)
  (send bm-dc set-brush transparent-brush)

  (send bm-dc set-pen   red4-pen)
  (send bm-dc draw-path (elliptical-arc-path 25 8 20 8 2.5 2.5 0 1 1)) ; large-arc, sweep
  (send bm-dc set-pen   blue4-pen)
  (send bm-dc draw-path (elliptical-arc-path 25 8 30 8 2.5 2.5 0 1 1))

  (send bm-dc set-pen    red4-pen)
  ;(send bm-dc draw-point x1 y1)
  ;(send bm-dc draw-point x2 y2)

  (send bm-dc set-pen    blue4-pen)
  ;(send bm-dc draw-point cx cy)
  bm
  )


; (elliptical-arc-path 650 325 700 300 25 25 -30 0 1)

;; (define bm    (make-object bitmap% 800 400))
;; (define bm-dc (new bitmap-dc% [bitmap bm]))
;; (send bm-dc set-smoothing 'smoothed)
;; (send bm-dc set-scale 1 1)
;; (send bm-dc draw-path (elliptical-arc-path 215 190  265 220   200 200  (* pi (/ 0 180)) 0 0))
;; ;(send bm-dc draw-path (elliptical-arc-path 215 190 265 190 40 200  (* pi (/ 0 180)) 0 0))
;; (send bm-dc set-pen (new pen% [color "red"] [width 4] [cap 'round] [join 'round]))
;; (send bm-dc draw-point 215 190)
;; (send bm-dc draw-point 265 190)
;; bm

