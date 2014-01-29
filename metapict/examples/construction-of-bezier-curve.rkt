#lang racket
(require metapict)

(define (draw-bezier-controls b)
  (defm (bez p0 p1 p2 p3) b)
  (penscale 4 (draw p0 p1 p2 p3)))

(define (draw-bezier-labels b l0 l1 l2 l3)
  (defm (bez p0 p1 p2 p3) b)
  (draw (label l0 p0 (bot))
        (label l1 p1 (top))
        (label l2 p2 (top))
        (label l3 p3 (bot))))

(define (draw-bezier-polygon b)
  (defm (bez p0 p1 p2 p3) b)
  (draw (curve p0 -- p1)
        (curve p2 -- p3)))

(define (draw-bezier-construction b t labels?)
  (define (l t) (if labels? t ""))
  (defm (bez p0 p1 p2 p3) b)
  (def q0 (med t p0 p1))
  (def q1 (med t p1 p2))
  (def q2 (med t p2 p3))
  (def r0 (med t q0 q1))
  (def r1 (med t q1 q2))
  (def p  (med t r0 r1))
  (draw (dot-label (l "R0") r0 (top))
        (dot-label (l "R1") r1)
        (dot-label (l "P")  p)
        (pencolor "green"
                  (draw (dot-label (l "Q0") q0 (lft))
                        (dot-label (l "Q1") q1 (top))
                        (dot-label (l "Q2") q2)
                        (curve q0 -- q1)
                        (curve q1 -- q2)))
        (pencolor "blue" (draw (curve r0 -- r1)))))

(def win (window -5 11 -5 11))
(def c (curve (pt 0 0) (vec -0.5 2) .. (vec -2 -2) (pt 5 0)))
(def b (first (curve:-bezs c)))
(defm (bez p0 p1 p2 p3) b)

(set-curve-pict-size 80 80)

(with-window win
  (for/list ([t (in-range 0 11/10 1/10)])
    (draw (pencolor "black" (draw-bezier-controls b))
          (draw-bezier-labels b "P0" "P1" "P2" "P3")
          (dashed (draw-bezier-polygon b))
          (draw-bezier-construction b t #f)
          (pencolor "red" (draw (subcurve c 0 t)))
          (pencolor "gray" (dotted (draw (subcurve c t 1))))
          (label (~a "t=" t) (pt+ (med 1/2 p0 p3) (vec* 2 down)) (bot)))))
  
  