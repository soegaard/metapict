#lang racket/base
;;;
;;; Function Plotting
;;;

(provide fun-graph)

(require racket/list racket/match racket/pretty
         "adaptive-plot.rkt"  "axis.rkt" "curve.rkt"
         "def.rkt" "domain.rkt" "draw.rkt" "function.rkt" "path.rkt"
         "system.rkt"
         (except-in "structs.rkt" open)
         (prefix-in adaptive- "adaptive-plot.rkt"))


(define (fun-graph s win the-fun
                   #:excluded-symbols [excluded '()] ; omit open closed symbols in this list
                   #:regions          [regions 9])
  #;(pretty-print
   (list 'fun-graph
         (list 's s)
         (list 'win win)
         (list 'fun the-fun)))
  ; window is in axis coordinates
  (defm (system: _ a1 a2) s)
  (defv (xmin xmax) (visible-range a1 win)) ; #f #f if out of range
  (defv (ymin ymax) (visible-range a2 win)) ; #f #f if out of range
  (unless (and xmin xmax ymin ymax)
    (define out (current-error-port))
    (displayln (list 'fun-graph 'x-range xmin xmax 'y-range ymin ymax) out)
    (displayln s out)
    (displayln win out)
    (displayln fun out)
    (error))
  
  (define (vec->point v)  (defm (vector x y) v) (point s x y))
  (define (component vs start-symbol end-symbol)
    (def ps (map point->pt (map vec->point vs)))
    (def spec (add-between ps ..))
    (attach-circles (curve* spec) start-symbol end-symbol))
  (define (end-symbol c? x)
    (match x
      [(or +inf.0 -inf.0) #f] ; none
      [_ 
       (if (member x excluded)
           #f ; none
           (match c?
             [#t 'closed]
             [#f 'open]))]))
  (match the-fun
    [(fun: name (domain intervals) f)
     (flatten
      (for/list ([i (in-list intervals)])
        #;(pretty-print
         (list 'fg
               (list 'interval-i i)
               (list 'xmin xmin ymin ymax)))
        (match i
          [(domain-interval ac? a b bc?)
           (define (excluded? x)
             (or (and (not ac?) (= x a))
                 (and (not bc?) (= x b))))
           (cond
             [(or (> a xmax) (< b xmin)) '()]
             [else (def axes? #f)
                   (def ptss (adaptive-plot2d f (max a xmin) (min b xmax) ymin ymax excluded? axes?
                                              #:regions regions))
                   ; (displayln (list 'a a 'xmin xmin 'max (max a xmin) 'a>=xmin (>= a xmin)))
                   (match ptss
                     [(list pts)               (list (component pts  (and (>= a xmin) (end-symbol ac? a))
                                                                     (and (<= b xmax) (end-symbol bc? b))))]
                     [(list pts0 pts1)         (list (component pts0 (and (>= a xmin) (end-symbol ac? a)) #f)
                                                     (component pts1 #f (and (<= b xmax) (end-symbol bc? b))))]
                     [(list pts0 pts ... pts1) (list (component pts0 (and (>= a xmin) (end-symbol ac? a)) #f)
                                                     (map (λ (pts) (component pts #f #f)) pts)
                                                     (component pts1 #f (and (<= b xmax) (end-symbol bc? b))))]
                     [(list)                   '()])])])))]))

;; (define (plot2d s f xmin xmax ymin ymax [excluded #f])
;;   ; list of lists of vectrs
;;   (def s  (system (pt 0 0) (vec .2 0) (vec 0 .2)))
;;   (define (vec->point v)  (defm (vector x y) v) (point s x y))
;;   (define (plot1 vecs)
;;     (define spec (add-between (map vec->point vecs) ---))
;;     (curve* spec))
;;   (map plot1 (adaptive-plot2d f xmin xmax ymin ymax excluded #f)))


;; (require "draw.rkt" "window.rkt" "curve.rkt" "path.rkt" "device.rkt")
;; (set-curve-pict-size 400 400)
;; (with-window (window -1 11 -1 11)
;;   (draw (plot2d g 0 3 -1 11)
;;         (plot2d g 3 9 -1 11 (λ(x) (or (= x 3) (= x 0.))))
;;         (curve (pt -10 0) -- (pt 10 0))
;;         (curve (pt 0 -100) -- (pt 0 10))))

; TODO: plot2d plots from +inf if (f xmin) is #f.


;(def c (plot2d g1 3.000 9 -1 11))
;(map curve-length c)


;(with-window (window 0.1 5 -5 5)
;  (draw (plot2d (λ (x) (* x x (sin (/ x)))) -1 5)))


;;; Examples


;; (require "draw.rkt" "window.rkt" "curve.rkt" "path.rkt" "device.rkt")
;; (set-curve-pict-size 400 400)
;; (def f (fun "f" (closed 2 4) (λ (x) (* x x))))
;; (procedure? f)

;; (require racket/math)

;; ; (def g1 (fun "g1" (open-closed 3 9) add1))

;; (def s (system (pt 0 0) (vec .2 0) (vec 0 .2)))
;; (defm (system: _ a1 a2) s)
;; (def w (window -2 2 -1 1)) 

;; (draw s 
;;       (ticks a1 #:size 0.1)   ; axis units
;;       (ticks a2 #:size 0.1)
;;       (fun-graph s w
;;         (cond-fun
;;          (list (list /                      (open -4 0))
;;                (list (λ(x) (+ (* .5 x) -3)) (closed-open  3 5))
;;                (list sqrt                   (closed-open  0 3)))))

;;       (fun-graph s w
;;                  (fun "sin" (open -inf.0 +inf.0) sin)))

