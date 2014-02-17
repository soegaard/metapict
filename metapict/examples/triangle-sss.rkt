#lang racket
(require metapict)

;;; Solution to problem from: 
;;;   http://tex.stackexchange.com/questions/142210

; In triangle ABC these sides are given:
(def a 8)
(def b 7)
(def c 6)

; Place BC horisontally with B=(0,0).
(def B  (pt 0 0))
(def BC (vec a 0))
(def C  (pt+ B BC))

; A is intersection point of circles with centers B and C
; and radii b and c respectively
(def circ-B (circle B b))
(def circ-C (circle C c))
(def A      (intersection-point circ-B circ-C))

; The point D is on the extension of BC.
(define (extend P Q) ; ray from P through Q and further
  (curve P -- (pt+ P (vec* 10 (pt- Q P)))))

; The problems states that angle(AB,AC)=angle(DC,DA).
(def AB (pt- B A))
(def AC (pt- C A))
(def α (angle2 AB AC))

; A vector parallel with AD:
(def kAD (pos (pt@ 20 (- α))))

; D is the intersection of the extension of BC and
; the ray from A through D.
(def D (intersection-point (extend B C) (extend A (pt+ A kAD))))

; Now A, B, C, and, D are determined, so we can draw the
; the situation.
(set-curve-pict-size 400 400)
(with-window (window -2 10 -2 10)
  (draw (color "gray" (draw circ-B circ-C))
        (curve A -- B -- C -- cycle)
        (curve A -- D -- B -- cycle)
        (label-top  "A" A)
        (label-llft "B" B)
        (label-bot  "C" C)
        (label-lrt  "D" D)))
