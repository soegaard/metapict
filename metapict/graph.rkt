#lang racket
(require racket/draw)
;;; 
;;; This draws simple graphs. 
;;; Use the plot library for more advanced graphs.
;;;



(provide graph ; draw the graph of a function 
         )

(require metapict "examples/pdf-experiment.rkt") 

(define graph-join ..)

(define (graph f [xmin #f] [xmax #f] #:samples [n 50] #:diff [df/dx #f])
  (defm (window Xmin Xmax Ymin Ymax) (curve-pict-window))
  (unless xmin  (set! xmin Xmin))
  (unless xmax  (set! xmax Xmax))
  (def ε 1.0e-10)
  (define (ndiff f x)
    (def x-ε (max xmin (- x ε)))
    (def x+ε (max xmin (+ x ε)))
    (/ (- (f x+ε) (f x-ε))
       (- x+ε x-ε)))
  (unless df/dx (set! df/dx (λ (x) (ndiff f x))))
  (def (φ x) (pt x (f x)))      ; x -> (x,f(x))
  (def (τ x) (vec 1 (df/dx x))) ; vector along tangent
  (def Δx (/ (- xmax xmin) n))
  (def x0 xmin)
  (def (xi i) (+ x0 (* i Δx)))
  (curve*
   (append (if (equal? graph-join --)
               (list (φ x0))
               (list (φ x0) (τ x0)))
           (for/list ([x (in-range (xi 1) (xi (+ n 1)) Δx)])
             (if (equal? graph-join --)
                 (list -- (φ x))
                 (list .. (φ x) (τ x)))))))

; this version builds Bezier curves directly
#;(define (graph f [xmin #f] [xmax #f] #:samples [n 50] #:diff [df/dx #f])
  (defm (window Xmin Xmax Ymin Ymax) (curve-pict-window))
  (unless xmin  (set! xmin Xmin))
  (unless xmax  (set! xmax Xmax))
  (def ε 1.0e-10)
  (define (ndiff f x)
    (def x-ε (max xmin (- x ε)))
    (def x+ε (max xmin (+ x ε)))
    (/ (- (f x+ε) (f x-ε))
       (- x+ε x-ε)))
  (unless df/dx (set! df/dx (λ (x) (ndiff f x))))
  (def (φ x) (pt x (f x)))      ; x -> (x,f(x))
  (def (τ x) (vec 1 (df/dx x))) ; vector along tangent
  (def Δx (/ (- xmax xmin) n))
  (def x0 xmin)
  (def (xi i) (+ x0 (* i Δx)))
  (curve: #f
          (for/list ([i (in-range 0 (+ n 1))])
            (def x0 (xi i))
            (def x3 (xi (+ i 1)))
            (bez (φ x0) 
                 (pt+ (φ x0) (vec* (/ Δx  3) (τ x0)))
                 (pt+ (φ x3) (vec* (/ Δx -3) (τ x3)))
                 (φ x3)))))

(require metapict/grid)
(set-curve-pict-size 300 300)

(let ()
  (defv (xmin xmax ymin ymax) (values -10 10 -10 10))
  (def win (window xmin xmax ymin ymax))
  (with-window win
    (draw (color "gray"    (grid (pt xmin ymin) (pt xmax ymax)))
          (color "red"     (draw (graph sqr #:diff (λ(x)(* 2. x)))))
          (color "blue"    (draw (graph sqr)))
          (color "green"   (draw (graph sqrt 0)))
          (color "magenta" (draw (graph sin)))
          (color "purple"  (draw (graph cos))))))

(let ()
  ; http://www.texample.net/tikz/examples/gnuplot-basics/
  (defv (xmin xmax ymin ymax) (values -0.5 6 -1.5 5))
  (def win (window xmin xmax ymin ymax))
  (with-window win
    (ahlength  (px 8))
    (def (f x) x)
    (def (g x) (* 1/20 (exp x)))
    (def (h x) (sin x))
    (draw (color "gray" (grid (pt -0.1 -1.1) (pt 3.9 3.9)))
          (draw-arrow (curve (pt -0.2 0) -- (pt 4.2 0))) (label-rt  "x" (pt 4.2 0))
          (draw-arrow (curve (pt 0 -1.2) -- (pt 0 4.2))) (label-top "y" (pt 0 4.2))
          (color "red"    (draw (graph f 0 4) (label-rt "f(x) = x"        (pt 4 (f 4)))))
          (color "orange" (draw (graph g 0 4) (label-rt "g(x) = 1/20 e^x" (pt 4 (g 4)))))
          (color "blue"   (draw (graph h 0 4) (label-rt "h(x) = sin(x)"   (pt 4 (h 4))))))))

(let ()
  ; draw parabola and sine - fill the area between them
  (defv (xmin xmax ymin ymax) (values -1.2 1.7 -1.2 1.7))
  (def win (window xmin xmax ymin ymax))
  (with-window win
    (ahlength  (px 8))
    (def (f x) (* x x))
    (def (g x) (sin x))
    (def F (graph f -1 1))
    (def G (graph g -1 1))
    (defm (list (pt x0 _) (pt x1 _)) (intersection-points F G))
    (define (cut-between c x0 x1)
      (def (vert x) (curve (pt x ymin) -- (pt x ymax)))
      (cut-after (cut-before c (vert x0)) (vert x1)))
    (draw (color "gray" (grid (pt -1.1 -1.1) (pt 1.1 1.1) #:step 0.2))
          (draw-arrow (curve (pt -1.2 0) -- (pt 1.2 0))) (label-rt  "x" (pt 1.2 0))
          (draw-arrow (curve (pt 0 -1.2) -- (pt 0 1.2))) (label-top "y" (pt 0 1.2))
          (color "yellow" (fill (curve-append (cut-between F x0 x1) 
                                              (cut-between G x0 x1))))
          (color "red"    (draw F (label-rt "f(x) = x^2"    (pt 1.1 (f 1)))))
          (color "orange" (draw G (label-rt "g(x) = sin(x)" (pt 1.1 (g 1))))))))

(let ()
  (def l 3) (def -l (- l))
  (def l+ (+  l 0.1)) (def l++ (+  l 0.2))
  (def l- (- -l 0.1)) (def l-- (- -l 0.2))
  (defv (xmin xmax ymin ymax) (values l-- l++ l-- l++))
  ; (defv (xmin xmax ymin ymax) (values 0 0.05 0 0.05))
  (def win (window xmin xmax ymin ymax))
  (with-window win
    (ahlength  (px 8))
    (def x-axis (draw (draw-arrow (curve (pt l-- 0) -- (pt l++ 0))) (label-rt  "x" (pt l++ 0))))
    (def y-axis (draw (draw-arrow (curve (pt 0 l--) -- (pt 0 l++))) (label-top "y" (pt 0 l++))))
    (def the-grid (color "gray" (grid (pt l- l-) (pt l+ l+) #:step 1)))
    (def (f x) (* x x))
    (def F (graph f -l l ; #:diff (λ(x) (* 2 x))
                  ))
    (def p (draw the-grid x-axis y-axis 
                 (color "red" (draw F (label-rt "f(x) = x^2" (pt l+ (f l)))))))
    (save-pict-as-pdf p "parabola.pdf")
    (displayln "Saved pict as parabola.pdf")
    p))