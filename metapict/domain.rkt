#lang racket/base
(require racket/contract racket/format racket/match
         racket/math racket/string racket/list
         "axis.rkt" "def.rkt" "device.rkt" "curve.rkt" "draw.rkt" "path.rkt"
         "pict.rkt" "pt-vec.rkt"  "structs.rkt" "parameters.rkt" "window.rkt")

;;;
;;; Domains (subsets of R)
;;;

; A domain interval represents an interval, a subset of the real number line
; of one of the following forms:
;    i)   a<x<b      open-open
;   ii)   a<=x<b   closed-open
;  iii)   a<x<=b     open-closed
;   iv)   a<=x<=b  closed-closed

; A domain consists of a list of intervals.
; The invariant is:
;   I)  the intervals doesn't overlap
;  II)  the intervals are sorted in ascending order

(provide
 ;;; DOMAIN INTERVALS
 an-empty-domain-interval
 empty-domain-interval?
 domain-interval-member?
 domain-interval-overlap?
 domain-interval<
 domain-interval-length
 ; constructors
 open-domain-interval
 closed-domain-interval
 oc-domain-interval
 co-domain-interval
 point-domain-interval

 ;;; DOMAINS
 ; values
 R R+ R0+ R- R0-
 ; constructors
 interval
 open
 closed
 open-closed
 closed-open
 ; operators
 domain-member?
 domain-interval-union
 domain-union
 domain-union*
 ;
 domain-length
 format-domain
 draw-domain
 attach-circles)

(define/contract (empty-domain-interval? I)
  (-> domain-interval? boolean?)
  (defm (domain-interval ac a b bc) I)
  (and (= a b) (not (and ac bc))))

(define/contract (domain-interval-member? x I)
  (-> real? domain-interval?   boolean?)
  (defm (domain-interval ac a b bc) I)
  (or (< a x b)
      (and ac (= x a))
      (and bc (= x b))))


(module+ test
  'interval-member?
  (and (eq? (domain-interval-member? 2 (domain-interval #f 1 3 #f)) #t)
       (eq? (domain-interval-member? 1 (domain-interval #f 1 3 #f)) #f)
       (eq? (domain-interval-member? 3 (domain-interval #f 1 3 #f)) #f)
       (eq? (domain-interval-member? 1 (domain-interval #t 1 3 #f)) #t)
       (eq? (domain-interval-member? 3 (domain-interval #f 1 3 #t)) #t)))

(define/contract (domain-member? x D)
  (-> real? domain?   boolean?)
  (for/or ([I (in-list (domain-intervals D))])
    (domain-interval-member? x I)))

(module+ test
  'domain-member?
  (and (not (domain-member? 2 (domain (list))))
       (domain-member? 2 (domain (list (domain-interval #f 1 3 #f))))
       (domain-member? 4 (domain (list (domain-interval #f 1 3 #f)
                                       (domain-interval #f 3 5 #f))))
       (not (domain-member? 3 (domain
                               (list (domain-interval #f 1 3 #f)
                                     (domain-interval #f 3 5 #f)))))))

(define/contract (domain-interval-length I)
  (-> domain-interval? number?)
  (defm (domain-interval ac a b bc) I)
  (if (or (infinite? a) (infinite? b))
      +inf.0
      (- b a)))

(define/contract (domain-length D)
  (-> domain? number?)
  (for/sum ([I (domain-intervals D)])
    (domain-interval-length I)))


(define/contract (domain-interval-overlap? I1 I2)
  (-> domain-interval? domain-interval?   boolean?)
  (defm (domain-interval ac a b bc) I1)
  (defm (domain-interval xc x y yc) I2)
  (or (or (< x a y)
          (and ac (or (and xc (= a x))
                      (and yc (= a y)))))
      (or (< x b y)
          (and bc (or (and xc (= a x))
                      (and yc (= a y)))))
      (or (< a x b)
          (and xc (or (and ac (= x a))
                      (and bc (= x b)))))
      (or (< a y b)
          (and yc (or (and ac (= y a))
                      (and bc (= y b)))))
      (and (= a x) (= b y) (not (= a b)))))


(module+ test  'interval-overlap?
         (let ()
           (define o   open-domain-interval)
           (define c closed-domain-interval)
  (and      (domain-interval-overlap? (o 1 3) (o   2 4))
            (domain-interval-overlap? (c 1 3) (c 3 4))
            (domain-interval-overlap? (c 3 4) (c 1 3))
       (not (domain-interval-overlap? (o 1 3) (c 3 4)))
       (not (domain-interval-overlap? (c 1 3) (o   3 4)))
       (not (domain-interval-overlap? (c 3 4) (o   1 3)))
       (not (domain-interval-overlap? (o 3 4) (c 1 3)))
            (domain-interval-overlap? (o 1 10) (o 3 4))
            (domain-interval-overlap? (o 3 4)  (o 1 10) )
       (not (domain-interval-overlap? (o -inf.0 3)      (o  3 +inf.0)))
       (not (domain-interval-overlap? (c -inf.0 3)      (o 3 +inf.0)))
       (not (domain-interval-overlap? (o -inf.0 3)      (c 3 +inf.0)))
       (domain-interval-overlap?      (c -inf.0 3)      (c 3 +inf.0))
       (domain-interval-overlap?      (o -inf.0 +inf.0) (o -inf.0 +inf.0))
       (domain-interval-overlap?      (c 42 42)         (c 42 42)))))

(define (open-domain-interval   a b)        (domain-interval #f a b #f))
(define (closed-domain-interval a b)        (domain-interval #t a b #t))
(define (oc-domain-interval     a b)        (domain-interval #f a b #t))
(define (co-domain-interval     a b)        (domain-interval #t a b #f))
(define (point-domain-interval a)           (closed-domain-interval a a))


(define (domain-interval< I1 I2) ; is all of I1 strictly less than all of I2
  (defm (domain-interval ac a b bc) I1)
  (defm (domain-interval xc x y yc) I2)
  (and (not (domain-interval-overlap? I1 I2))
       (or (< b x)
           (and (= b x) (not (and bc xc))))))

(module+ test
  'domain-interval<
         (let ()
           (define o   open-domain-interval)
           (define c closed-domain-interval)
  (and (domain-interval< (o 1 2) (o   2 3))
       (domain-interval< (o 1 2) (c 2 3))
       (domain-interval< (c 1 2) (o 2 3))
       (not (domain-interval< (c 1 2) (c 2 3))))))

(define (domain-interval-union I1 I2)
  (defm (domain-interval ac a b bc) I1)
  (defm (domain-interval xc x y yc) I2)
  (unless (domain-interval-overlap? I1 I2)
    (error 'domain-interval-union
           (~a "expected overlapping intervals, got: " I1 " and " I2)))
  (define maximum (max b y))
  (define minimum (min a x))
  (domain-interval (or (and ac (= a minimum))
                       (and xc (= x minimum)))
                   minimum maximum
                   (or (and bc (= b maximum))
                       (and yc (= y maximum)))))

(define/contract (two-intervals->domain I1 I2)
  (-> domain-interval? domain-interval?   domain?)
  (defm (domain-interval ac a b bc) I1)
  (defm (domain-interval xc x y yc) I2)
  (cond
    [(empty-domain-interval? I1) I2]
    [(empty-domain-interval? I2)  I1]
    [(domain-interval< I1 I2)         (domain (list I1 I2))]
    [(domain-interval< I2 I1)         (domain (list I2 I1))]
    [(domain-interval-overlap? I1 I2)
     (domain (list (domain-interval-union I1 I2)))]
    [else (error 'two-intervals->domain (~a "got: "  I1 " and " I2))]))


(define an-empty-domain-interval (open-domain-interval 0 0))

(define/contract (domain-intervals->domain is)
  (-> (listof domain-interval?)  domain?)
  (match is
    [(list)        (domain (list an-empty-domain-interval))]
    [(list I)      (domain (list I))]
    [(list I1 I2)  (two-intervals->domain I1 I2)]
    [(cons I1 Is)  (two-intervals->domain I1 (domain-intervals->domain Is))]
    [_ (error 'domain-intervals->domain (~a "got: " is))]))

#;(define (insert-domain-interval-into-intervals i js)
    (match js
      [(list)       (list i)]
      [(cons j js)  (cond [(domain-interval-overlap? i j)
                           (cons (domain-interval-union i j) js)]
                          [(domain-interval< i j)
                           (cons i (cons j js))]
                          [else
                      (cons j (insert-domin-interval-into-intervals i js))])]))

(define/contract (merge-domain-intervals is* js*)
  (-> (listof domain-interval?) (listof domain-interval?)
      (listof domain-interval?))
  (define merge merge-domain-intervals)
  ; the lists are sorted
  (match* (is* js*)
    [('() js)  js*]
    [(is '())  is*]
    [( (cons i is) (cons j js) )
     (cond [(domain-interval< i j)         (cons i (merge is js*))]
           [(domain-interval< j i)         (cons j (merge is* js))]
           [(domain-interval-overlap? i j) (cons (domain-interval-union i j)
                                                 (merge is js))])]))

(define (domain-union d1 d2)
  (defm (domain is) d1)
  (defm (domain js) d2)
  (domain (merge-domain-intervals is js)))

(define (domain-union* ds)
  (match ds
    ['()              an-empty-domain-interval]
    [(list d1)        d1]
    [(list d1 d2)     (domain-union d1 d2)]
    [(list* d1 d2 ds) (domain-union* (cons (domain-union d1 d2) ds))]
    [_ (error 'domain-union* (~a "expected a list of domains, got: " ds))]))



(define (interval a b [ac #f] [bc #f])
  (unless (<= a b)
    (error 'interval (~a "expected a<=b, got: " a " and " b)))
  (domain (list (domain-interval ac a b bc))))

(define (closed a b)      (interval a b #t #t))
(define (open a b)        (interval a b #f #f))
(define (open-closed a b) (interval a b #f #t))
(define (closed-open a b) (interval a b #t #f))
; Common intervals
(define R   (open -inf.0 +inf.0))
(define R+  (open             0 +inf.0))
(define R0+ (closed-open      0 +inf.0))
(define R-  (open        -inf.0      0))
(define R0- (open-closed -inf.0      0))



(define (format-interval i)
  (define (~ x)
    (if (not (infinite? x))
        (~a x)
        (if (positive? x)
            "∞"
            "-∞")))    
  (defm (domain-interval ac a b bc) i)
  (~a (if ac "[" "]")
      (~ a) "," (~ b)
      (if bc "]" "[")))

(define (format-domain d)
  (defm (domain intervals) d)
  (def  is (for/list ([i intervals])
             (format-interval i)))
  (string-append* (add-between is "U")))

(define (draw-domain-interval i 
                               #:offset-vec v
                               #:direction  d
                               #:color col)
  (define (symbol xc x)
    (case x
      [(-inf.0 +inf.0) #f]
      [else            (if xc 'closed 'open)]))
  (defm (domain-interval ac a b bc) i)
  (def A (pt+ (pt+ origo (vec* a d)) v))
  (def B (pt+ (pt+ origo (vec* b d)) v))
  (attach-circles (curve A -- B)
                  (symbol ac a)
                  (symbol bc b)))

(define (draw-domain d
                     #:axis   [a #f]
                     #:offset [offset 0]
                     #:color  [col (get current-domain-color)])
  (defm (domain intervals) d)
  (for/draw ([i intervals])
            (draw-domain-interval
             i
             #:color col
             #:offset-vec (vec 0 offset)
             #:direction  (vec 1 0))))

(require "shapes.rkt")

(define (attach-circles c start end
                        #:size [size 3])
  ; Note: The size of the circles are in device coordinates.  
  (define (shape x pos) (if x (circle pos size) (curve pos)))
  (define (make-draw x)
    (case x
      [(open)   draw]
      [(closed) fill]
      [else     (λ x #f)]))
  (def A (start-point c))
  (def B (end-point   c))
  (def T (current-curve-transformation))
  (def sA (shape start (T A)))
  (def sB (shape end   (T B)))
  (with-device-window
    (draw ((make-draw start) sA)
          (cut-after (cut-before (T c) sA) sB)
          ((make-draw end)   sB))))


#;(

(attach-circles (curve (pt 0 0) -- (pt 1/2 1/2))
                'open 'closed)

(attach-circles (curve (pt 0 0) -- (pt 1/2 1/2))
                'open #f)

#;(draw-domain (domain-union (domain-union (open-closed -1 -1/2)
                                           (closed-open 0 1/2))
                             (open 3/4 +inf.0)))

(draw-domain (open-closed -1 -1/2))
(draw-domain (open-closed  0  1/2))
;(draw-domain (open-closed  3/4  +inf.0)) ; bug

(draw-domain (domain-union (open-closed -1 0)
                           (open-closed  0.3  1/2)))
)
