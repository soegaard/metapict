#lang racket
(require metapict) 

(define (graph f df/dx [n 20] [xmin -10] [xmax 10])
  (def (φ x) (pt x (f x)))      ; x -> (x,f(x))
  (def (τ x) (vec 1 (df/dx x))) ; vector along tangent
  (def Δx (/ (- xmax xmin) n))
  (def x0 xmin)
  (def (xi i) (+ x0 (* i Δx)))
  (curve*
   (append (list (φ x0) (τ x0))
          (for/list ([x (in-range (xi 1) (xi (+ n 1)) Δx)])
            (list .. (φ x) (τ x))))))

(set-curve-pict-size 300 300)
(with-window (window -10 10 0 20)
  (draw (graph sqr (λ(x)(* 2. x)))))

  