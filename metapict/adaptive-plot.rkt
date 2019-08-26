#lang racket
(require metapict)
(provide plot2d)

;;; This implements plotting a 2d graph of a one variable function.
;;; Singularities are handled properly (I think).

;;; The points evaluated are chosen using an adaptive strategy:
;;;    - in areas where the graph is smooth, few points are chosen
;;;    - in areas where the graph is oscillating, more points are chosen

;;; See http://goo.gl/Hdi9q   (plot.lisp in the Maxima source)
;;; for code implementing the adaptive strategy.

;;; See chapter 4.1 in the YACAS book of algorithms for
;;; an explanation of the algorithm:
;;;   http://yacas.sourceforge.net/Algochapter4.html#c4s1

;;; See 
;;; http://scicomp.stackexchange.com/questions/2377/algorithms-for-adaptive-function-plotting
;;; for a discussion on adaptive plotting.

(define *debug* #f)

; These count functions are used to measure the
; number of evaluations of the function to be drawn.
; They are only used during debugging.
(define count 0)
(define (reset-count) 
  (set! count 0))
(define (increase-count) 
  (set! count (+ count 1)))

; Plotting an arbitrary function without knowledge of any
; points in the domain where the function can be undefined
; risks triggering various exceptions. Handle these and 
; return #f to signal x is not in the domain.
(define (wrap f excluded-from-domain? [value-returned-on-error #f])
  (λ (x)
    (with-handlers ([(λ e #t) (λ x value-returned-on-error)])
      (increase-count)
      (if (excluded-from-domain? x)
          value-returned-on-error
          (f x)))))

(define (hill-or-valley? a b c)
  ; (x1,a) (x2,b) (x3,c) points with x1<x2<x3
  ; Check whether the middle number is below 
  ; or above the two other numbers.
  (or (and (> a b) (< b c))    ; valley
      (and (< a b) (> b c))))  ; hill

(define (oscilates? a b c d e)
  ; (x1,a) (x2,b) (x3,c) (x4,d) (x5,e) 
  ; points with x1<x2<x3<x4<x5
  (or
   ; f undefined somewhere?
   (not (andmap number? (list a b c d e)))
   ; or irregular shape?
   (and (hill-or-valley? a b c)
        (hill-or-valley? b c d)
        (hill-or-valley? c d e))))

(define (smooth? fa fa1 fb fb1 fc eps)
  ; fa fa1 fb fb1 fc are function values
  ; in the points a < a1 < b < b1 < c.
  ; eps is the relative error of the numerical
  ; approximation of the integral below f.
  (define machine-epsilon 2.2204460492503131e-16)
  (and (andmap number? (list fa fa1 fb fb1 fc))
       ; compare two approximations of the area of
       ;    f(x)-min{fa, fa1,fb, fb1,fc}
       ; If the two approximations agree, 
       ; the function is smooth.
       (<= (- (abs (/ (+ fa (* -5. fa1) (* 9. fb) (* -7. fb1) (* 2. fc)) 24.))
              (* eps (- (+ (* 5. fb) (* 8. fb1) (- fc))
                        (min fa fa1 fb fb1 fc))))
           ; this handles rounding problems 
           ; see plot.list source says (* 10 ...)
           (* 5 machine-epsilon))))

(define current-max-bend (make-parameter 0.5))

(define (adaptive f a b c fa fb fc d eps)
  ; (display d)
  ; d is maximal depth 
  ; eps is the relative area error (see smooth?)
  ; At most 2^d additional points will be generated  
  
  ; 1. a < a1 < b < b1 < c
  (define a1 (/ (+ a b) 2.0))
  (define b1 (/ (+ b c) 2.0))
  (define fa1 (f a1))
  (define fb1 (f b1))
  ; 2. 
  (if (or (<= d 0)
          (and (not (oscilates? fa fa1 fb fb1 fc)) ; 3.
               (let ([bends
                      (and (andmap number? (list fa fa1 fb fb1 fc))
                           (<= (bend-angle a fa a1 fa1 b fb1)  (current-max-bend))
                           (<= (bend-angle a1 fa1 b fb b1 fb1) (current-max-bend))
                           (<= (bend-angle b fb b1 fb1 c fc)   (current-max-bend)))])
                 #;(when bends
                   (displayln (list (bend-angle a fa a1 fa1 b fb1)
                                    (bend-angle a1 fa1 b fb b1 fb1)
                                    (bend-angle b fb b1 fb1 c fc)
                                    (<= (bend-angle a fa a1 fa1 b fb1)  (current-max-bend))
                                    (<= (bend-angle a1 fa1 b fb b1 fb1) (current-max-bend))
                                    (<= (bend-angle b fb b1 fb1 c fc)   (current-max-bend))
                                    bends)))
                 ; (displayln bends)
                 bends)
               (smooth? fa fa1 fb fb1 fc eps)      ; 4.
               ))
      ; don't refine
      (begin
        ; (when (not (= d 0)) (displayln "!"))
        (list (vector a fa) (vector a1 fa1) (vector b fb) 
              (vector b1 fb1) (vector c fc)))
      ; refine
      (let* ([eps2 (* 2.0 eps)]
             [left  (adaptive f a a1 b fa fa1 fb (- d 1) eps2)]
             [right (adaptive f b b1 c fb fb1 fc (- d 1) eps2)])
        (append left (rest right)))))



(define (region f a c [d 4] [eps (/ (* 400 400))])
  ; handles the region between a and c 
  ; a good value for eps is 1/(screen_area)
  (define b (/ (+ a c) 2.))  
  (adaptive f a b c (f a) (f b) (f c) d eps))

; split-between : pred xs -> (list (list x ...) ...)
;   The list xs is split into sublists.
;   For all neighbors x1 and x2 in xs, (pred x1 x2) determines whether the list is split.
;   Example:  (split-between = (list 1 1 2 3 3))  =>  '((1 1) (2) (3 3))
(define (split-between pred xs)
  (let loop ([xs xs] [ys '()] [xss '()])
    (match xs
      [(list)                 (reverse (cons (reverse ys) xss))]
      [(list x)               (reverse (cons (reverse (cons x ys)) xss))]
      [(list x1 x2 more ...)  (if (pred x1 x2) 
                                  (loop more (list x2) (cons (reverse (cons x1 ys)) xss))
                                  (loop (cons x2 more) (cons x1 ys) xss))])))

(define (split-at-false xs)
  (match xs
    ['()                        '()]
    [(list* (vector _ #f) more) (split-at-false more)]
    [(list* xs)                 (defv (ys zs) (splitf-at xs (λ (v) (vector-ref v 1))))
                                (cons ys (split-at-false zs))]))

(define (cons-if bool x xs)
  (if bool (cons x xs) xs))

(define (plot2d unwrapped-f [x-min -5] [x-max 5] [y-min -5] [y-max 5] [excluded? #f] [axes? #t])
  ; wrap the function to be drawn, s.t. it 
  ; returns #f in error situations
  (define (excluded-from-domain? x)
    (and excluded?
         (excluded? x)))
  
  (define f (wrap unwrapped-f excluded-from-domain? #f))  
  (define (clipped-to-different-sides? p q)
    ; are the points p and q on different
    ; sides of the y-min / y-max limit ?
    (define py (vector-ref p 1))
    (define qy (vector-ref q 1))
    (or (not (number? py))
        (not (number? qy))
        (and (< py y-min) (> qy y-max))
        (and (> py y-max) (< qy y-min))))
  (define (remove-non-numbers ps)
    (filter (λ (p) (number? (vector-ref p 1))) ps))
  ; 29 is a good value according to plot.lisp
  (define number-of-regions 29) 
  ; region width
  (define delta (/ (- x-max x-min) number-of-regions))
  ; generate points by dividing the interval
  ; from x-min to x-max into number-of-regions regions,
  ; and calling region, which calls adaptive.
  (define points  ; list of (vector x y) where y might be #f
    (append*
     (for/list ([i number-of-regions])
       (define a (+ x-min (* delta i)))
       ; avoid rounding error and compute c as:
       (define c (+ x-min (* delta (+ i 1))))
       (if (= i 0)
           ; keep the first point for the first region
           (region f a c) 
           ; otherwise remove the first point (which is 
           ; present as the end point of the preceding region)
           (rest (region f a c))))))

  
  ; Split the point list into groups. Split between two points 
  ; if they are on different sides of y-min and y-max.
  ; All connected points will be drawn with (lines ...).
  (define connected-points
    (append*
     (map (λ (points) (split-between clipped-to-different-sides? points))
          (split-at-false points))))
    
  ; Display both the adaptive plot and the original plot
  ; for visual comparision.
  (define (vector->pt v) (pt (vector-ref v 0) (vector-ref v 1)))
  (define (vectors->list vs) (map vector->pt vs))
  
  ; (displayln (map vectors->list connected-points))
  
  (map curve*
       (map (λ (ps) (add-between ps --))
            (map vectors->list connected-points)))
  
  #;(if *debug*  
      (begin
      (displayln
       (list
        (begin0
          (plot (list 
                 (cons-if axes? (axes)
                          (map lines 
                               (map remove-non-numbers 
                                    connected-points))))
                #:x-min x-min #:x-max x-max 
                #:y-min y-min #:y-max y-max)
          (displayln (format "adaptive number of evaluations: ~a" count))
          (reset-count))
        (plot (function (wrap unwrapped-f (λ (x) #f) +inf.0) x-min x-max)
              #:x-min x-min #:x-max x-max 
              #:y-min y-min #:y-max y-max)))
      (displayln (format "original number of evaluations: ~a\n\n" count))
      (reset-count))
    
    (plot (list 
           (cons-if axes? (axes)
                    (map lines 
                         (map remove-non-numbers 
                              connected-points))))
          #:x-min x-min #:x-max x-max 
          #:y-min y-min #:y-max y-max)))

#;(module* test #f
  ; Examples
  (plot2d (λ(x) 2))
  (plot2d (λ (x) x))
  (plot2d (λ (x) (* 5 x)))
  (plot2d /)     ; check division by zero and singularity at x=0
  (plot2d tan)   ; check multiple singularities
  (plot2d (λ (x) (- (/ x))))   ; check symmetry
  (plot2d (λ (x) (/ (abs x)))) ; check connected components of same sign
  (plot2d (λ (x) (* 1e6 x)))   ; check that lines with high slopes are drawn
  (plot2d (λ (x) (* 1e50 x)))   ; check that lines with high slopes are drawn
  (plot2d (λ (x) (tan x)))
  (plot2d (λ (x) (log (+ 1 (sin (cos x))))) -6. 6. -10. 10.)
  
  (plot2d (λ (x)  (+ (sin (expt x 3)) (cos (expt x 3)))) 0. 6.28 -1.5 1.5)
  
  ; This one should really be improved upon:
  (plot2d (λ (x) (sin x)) -5. 200. -1. 1.)
  
  ; horror examples from 
  ;    www.mines-paristech.fr/~kajler/pub/9507-RAOBNK-issac95.ps
  (plot2d (λ (x) (sin (/ x))) -2 2 -1 1)
  (plot2d (λ (x) (sin (expt x 4))) -4 4 -1 1)
  (plot2d (λ (x) (sin (* 300 x))) -4 4 -1 1)
  (plot2d (λ (x) (+ 1 (* x x) (* 0.0125 (log (abs (- 1 (* 3 (- x 1)))))))) -2 2 0 3)
  (plot2d (λ (x) (sin (exp x))) -6. 6. -1. 1.)
  (plot2d (λ (x) (/ (sin x))) -10. 10. 0. 10.)
  (plot2d (λ (x) (/ (sin x) x)) -6. 6. 0. 2.)
  
  (plot2d (λ (x) (+ (tan (+ (expt x 3) (- x) 1)) (/ (+ x (* 3 (exp x)))))) -2. 2. -15. 15.)
  ; exclusions help, but there are still problems:
  (plot2d (λ (x) (+ (tan (+ (expt x 3) (- x) 1)) (/ (+ x (* 3 (exp x)))))) -2. 2. -15. 15.
          (λ (x) (or (= (cos (+ (expt x 3) (- x) 1)) 0)
                     (= (+ x (* 3 (exp x))) 0)))))

(define (bend-angle x1 y1 x2 y2 x3 y3)
  (define (norm x y) (sqrt (+ (sqr x) (sqr y))))
  (cond
    [(= (- (max y1 y2 y3) (min y1 y2 y3)) 0) 0]
    [else
     (define abx (/ (- x2 x1) (norm (- x2 x1) (- y2 y1))))
     (define aby (/ (- y2 y1) (norm (- x3 x2) (- y3 y2))))
     (define bcx (/ (- x3 x2) (norm (- x2 x1) (- y2 y1))))
     (define bcy (/ (- y3 y2) (norm (- x3 x2) (- y3 y2))))
     (define angle (acos (min 1.0 (max 0.0 (+ (* abx bcx) (* aby bcy))))))
     #;(when  (> angle 0.30)
         (displayln (list (list 'bend-angle x1 y1 x2 y2 x3 y3)
                          (list 'scaled (list abx aby) (list bcx bcy))
                          (list 
                           (list 'angle: angle)))))
     angle]))
