#lang racket/base
;;;
;;; Parametric Plot
;;;

(require racket/math racket/draw metapict)

(provide parametric-plot) ; draw the graph of a function 


(define graph-join --)

(define (parametric-plot fx fy
                         tmin tmax
                         #:samples [n 50]
                         #:diffx [df/dx #f]
                         #:diffy [df/dy #f])
  
  (def ε 1.0e-10)
  (define (ndiff f t)
    (def t-ε (max tmin (- t ε)))
    (def t+ε (min tmax (+ t ε)))
    (/ (- (f t+ε) (f t-ε))
       (- t+ε t-ε)))

  (unless df/dx (set! df/dx (λ (t) (ndiff fx t))))
  (unless df/dy (set! df/dy (λ (t) (ndiff fy t))))
  
  (def (φ t) (pt (fx t) (fy t)))         ; t -> (fx(t),fy(t))
  (def (τ t) (vec (df/dx t) (df/dy t)))  ; vector along tangent
  (def Δt (/ (- tmax tmin) n))
  (def t0 tmin)
  (def (ti i) (+ t0 (* i Δt)))
  (def (xi i) (fx (ti i)))
  (def (yi i) (fy (ti i)))
  (curve*
   (append (if (equal? graph-join --)
               (list (φ t0))
               (list (φ t0) (τ t0)))
           (for/list ([t (in-range (ti 1) (ti (+ n 1)) Δt)])
             (if (equal? graph-join --)
                 (list -- (φ t))
                 (list .. (φ t) (τ t)))))))

;; (define (window-outline win)
;;   (defv (p q) (window-opposite-corners win))
;;   (rectangle p q))

;; (define (clip-to-window c [win (curve-pict-window)])
;;   (clipped (window-outline win) c))


(module+ test 

(require metapict)
(require metapict/grid)
(set-curve-pict-size 301 301)

(let ()
  (defv (xmin xmax ymin ymax) (values -10 10 -10 10))
  (def win (window xmin xmax ymin ymax))
  (with-window win
    (draw (color "gray"    (grid (pt xmin ymin) (pt xmax ymax)))
          (color "red"     (draw (parametric-plot (λ (t) (expt t 2)) (λ (t) (expt t 3)) -5 5))))))


(let ()
  (defv (xmin xmax ymin ymax) (values -16 16 -16 16))
  (def win (window xmin xmax ymin ymax))
  (def (fx t) (+ (* 10 (cos t)) (* 5 (cos (* -3 t)))))
  (def (fy t) (+ (* 10 (sin t)) (* 5 (sin (* -3 t)))))
  (with-window win
    (draw (color "gray"    (grid (pt xmin ymin) (pt xmax ymax) #:step 2))
          (color "red"     (draw (parametric-plot fx fy 0 (* 2 3.14) #:samples 200))))))

(let ()
  (defv (xmin xmax ymin ymax) (values -1.1 1.1 -1.1 1.1))
  (def win (window xmin xmax ymin ymax))
  (def (fx t) (sin t))
  (def (fy t) (sin (* 2 t)))
  (with-window win
    (draw (color "gray"    (grid (pt xmin ymin) (pt xmax ymax) #:step 2))
          (color "red"     (draw (parametric-plot fx fy 0 (* 2 3.14) #:samples 200))))))

(let ()
  (defv (xmin xmax ymin ymax) (values -2.1 2.1 -2.1 2.1))
  (def win (window xmin xmax ymin ymax))
  (def fxs  (list (λ (t) (* 2 (cos t)))
                  (λ (t) (* 2 (cos t)))
                  (λ (t) (cos t))
                  (λ (t) (cos t))))
  (def fys  (list (λ (t) (* 2 (sin t)))
                  (λ (t) (sin t))
                  (λ (t) (* 2 (sin t)))
                  (λ (t) (sin t))))
  (def cols (list "blue" "yellow" "green" "red"))
  (def (fy t) (sin (* 2 t)))
  (with-window win
    (draw (color "gray"    (grid (pt xmin ymin) (pt xmax ymax) #:step 1.))
          (color "red"     (draw* (for/list ([fx fxs] [fy fys] [col cols])
                                    (color col
                                           (draw
                                            (parametric-plot fx fy 0 (* 2 3.14) #:samples 200)))))))))


  (require racket/format)

  (let ()
    (defv (xmin xmax ymin ymax) (values -1.5 1.5 -1.5 1.5))
    (def win (window xmin xmax ymin ymax))
    
    (def (fx t) (sin t))
    (def (fy t) (sin (* 2 t)))
    (defv (tmin tmax) (values 0 (* 2 3.14)))

    (def ε 1.0e-10)
    (define (ndiff f t)
      (def t-ε (max tmin (- t ε)))
      (def t+ε (min tmax (+ t ε)))
      (/ (- (f t+ε) (f t-ε))
         (- t+ε t-ε)))

    (def t0 0.5)
    (def (P t) (pt (fx t) (fy t)))
    (def P0 (P t0))  
    (def (lab t [dir (cnt)]) (draw (label (~a "t=" (~r t)) (pt+ (P t) (vec 0.05 0)) (rt))))
    (def (dP/dt t) (vec (ndiff fx t) (ndiff fy t)))
    (with-window win
      (draw (color "gray"    (grid (pt xmin ymin) (pt xmax ymax) #:step 2))
            (color "red"     (draw (parametric-plot fx fy 0 (* 2 3.14) #:samples 200)))
            (draw (label (~a "t = " t0) (pt 0 1.3) (cnt)))
            (draw* (for/list ([t (in-range 0 (* 2 3.14) 0.5)])
                     (list (penscale 4 (color "blue" (draw (P t))))
                           (lab t (rot-90 (unit-vec (dP/dt t))))))))))
  )
  



