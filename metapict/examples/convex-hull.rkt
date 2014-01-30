#lang racket
; TODO : currently convex-hull can't handle repeated points in the input
(require ; "pt-vec.rkt"
  metapict)

(module+ test (require rackunit)
  (def a (pt 0 0))
  (def b (pt 0 1))
  (def c (pt 1 1))
  (def d (pt 1 0))
  (def e (pt .5 .5)))

; leftist : pt pt -> pt
;   Return the leftmost point.
;   Use the topmost point in case of a tie.
(define (leftist p q)
  (match* (p q)
    [((pt px py) (pt qx qy))
     (if (or (< px qx) (and (= px qx) (> py qy)))
         p q)]))



; left-most-point : (listof pt) -> pt
;   find the leftmost, topmost point
(define (left-most-point ps)
  (for/fold ([pmin (first ps)]) ([p (in-list (rest ps))])
    (leftist pmin p)))

(module+ test (require rackunit)
  (check-equal? (left-most-point (list a b c d e)) b))


; turn-kind : pt pt pt -> (one-of 'left 'right 'straight)
;   Determine if p..q..r is a left (counter clockwise) turn,
;   or a right (clockwise) turn.
(define (turn-kind p q r)
  (def pq (pt- q p))
  (def qr (pt- r q))
  (case (exact->inexact (sgn (dot pq (rot90 qr))))
    [(1.0)  'left]
    [(0.0)  'straight]
    [(-1.0) 'right]
    [else (error 'turn-kind (~a "got p q r " (sgn (dot pq (rot90 qr)))))]))

(module+ test
  (check-equal? (turn-kind a b c) 'left)
  (check-equal? (turn-kind b c d) 'left)
  (check-equal? (turn-kind c d a) 'left)
  (check-equal? (turn-kind d a b) 'left)
  (check-equal? (turn-kind a d c) 'right)
  (check-equal? (turn-kind d c b) 'right)
  (check-equal? (turn-kind c b a) 'right)
  (check-equal? (turn-kind b a d) 'right)
  (check-equal? (turn-kind a e c) 'straight)
  (check-equal? (turn-kind c e a) 'straight)
  (check-equal? (turn-kind e c d) 'left)
  (check-equal? (turn-kind e d c) 'right))

(define (collinear? p q r)
  (defv (pq pr) (values (pt- q p) (pt- r p)))
  (or (zero? (norm pq))
      (zero? (norm pr))
      (<= (abs (- (angle pq) (angle pr))) 0.000000001)))

(define (nearest p q r)
  (if (< (norm (pt- q p)) (norm (pt- r p))) q r))

; right-most-wrt : pt (list-of pt) -> pt
;   find the right most pt in S wrt to p
(define (right-most-wrt p ps)
  (define (right-most q r) 
    (cond [(collinear? p q r)            (nearest p q r)]
          [(eq? (turn-kind p q r) 'right) r]
          [else                           q]))
  (def rs (remove p ps))
  (for/fold ([q (first rs)]) ([r (in-list (rest rs))])
    (right-most q r)))

(module+ test 
  (def all (list a b c d e))
  (check-equal? (right-most-wrt a (list b)) b)
  (check-equal? (right-most-wrt a all) b)
  (check-equal? (right-most-wrt b all) c)
  (check-equal? (right-most-wrt c all) d)
  (check-equal? (right-most-wrt d all) a))

(define (convex-hull ps) ; gift wrapping algorithm
  ; the left most point lie on the convex hull
  (def p0 (left-most-point ps))
  (define (loop p)
    (def q (right-most-wrt p ps))
    (cond [(eq? p0 q) '()]
          [else       (cons q (loop q))]))
  (cons p0 (loop p0)))

(def a (pt 0 0))
(def b (pt 0 1))
(def c (pt 1 1))
(def d (pt 1 0))
(def e (pt .5 .5))

(convex-hull (list a b c d e))

(define (add-points-between ps)
  (match ps
    [(list* p q rs)
     (cons p 
           (cons (pt+ (med p q) (vec (random 2) (random 2)))
                 (add-points-between (rest ps))))]))


(define (race-track)
  (with-window (window -15 15 -15 15)
    (def pts (for/list ([n 7]) (pt (random 10) (random 10))))  
    (displayln pts)
    (scale (draw (penscale 3 (draw* pts))
                 (color "red" (draw (curve* (append (add-between (convex-hull pts) ..)
                                                    (list .. cycle))))))
           4)))
