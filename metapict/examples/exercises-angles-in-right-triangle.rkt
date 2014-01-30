#lang racket
(require metapict math/base racket/math)

;;;
;;; EXAMPLE: The file generates a random right-angled triangle.
;;;

(define (random-letters)
  (def first-letter (string-ref "ads" (random 3)))
  (def small-letters (for/list ([i 3]) (~a (integer->char (+ (char->integer first-letter) i)))))
  (def large-letters (map string-upcase small-letters))
  (values small-letters large-letters))

; return unit vector in the direction of v
(define (unit v) (vec* (/ (norm v)) v))

(define (device-right-angle A B C size)
  ; Given points A, B and C in device coordinates,
  ; return a curve (in device coordinates) for a right angle marker
  ; for the angle at B in angle ABC.
  (def BA (vec* size (unit (pt- A B))))
  (def BC (vec* size (unit (pt- C B))))
  (curve    (pt+ B BA) 
         -- (pt+ (pt+ B (vec+ BA BC)))
         -- (pt+ B BC)))

(define (right-angle-marker A B C size)
  ; Given points A,B, and, C in logical coordinates
  ; and a size in device units, return a curve in logical coordinates
  ; for the right angle marker for angle B in angle ABC.
  (def T (current-curve-transformation)) ; logical -> device
  ((inverse T) (device-right-angle (T A) (T B) (T C) size)))

(define (device-angle-marker A B C r)
  ; Return curve of angle marker for angle ABC.
  ; Orientation from BA to BC is assumed positive.
  ; All coordinates are device coordinates.
  ; r is the radius of the marker
  (def BA (vec* r (unit (pt- A B))))
  (def BC (vec* r (unit (pt- C B))))
  (defm (pt x0 y0) B) ; center of marker
  ; todo : extend angle to accept points
  (displayln (list BA (deg (angle BA)) BC (deg (angle BC))))
  (def a (shifted x0 y0 (arc r (angle BA) (angle BC))))
  (def a0 (point-of a 0))                ; first point
  (def a1 (point-of a (curve-length a))) ; last point
  (curve-append (curve B -- a0) (curve-append a (curve a1 -- cycle))))

(define (angle-marker A B C r)
  ; Given points A,B, and, C in logical coordinates
  ; and a size r in device units, return a curve in logical coordinates
  ; for the angle indicator for the angle ABC.
  (def T (flipy (current-curve-transformation))) ; logical -> device
  ; the flipy keeps a positive orientation, s.t. angles work as expected
  ((inverse T) (device-angle-marker (T A) (T B) (T C) r)))

(define (sine-exercise)
  (defv (sides angles) (random-letters))
  (def opp (random-integer 1 10))
  (def hyp (+ opp (random-integer 1 5)))
  (def adj (sqrt (- (sqr hyp) (sqr opp))))
  (def ang (radians->degrees (asin (/ opp hyp))))
  (def A (pt 0 0))
  (def B (pt@ hyp (rad ang)))
  (def C (pt adj 0))
  (with-window (window -1 (+ adj 1) -1 (+ opp 1))
    (inset 
     (draw (curve A -- C -- B -- cycle)
           (label-rt   (~a opp) (med .5 B C))
           (label-ulft (~a hyp) (med .5 A B))
           (right-angle-marker A C B 5)
           (angle-marker C A B 10))
     10)))

(sine-exercise)


