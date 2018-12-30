#lang racket
(provide solve-cubic)
(require math/flonum)

; Other References:
;     https://courses.cs.washington.edu/courses/cse590b/13au/

;;;
;;; Cubic Equations
;;;

;; This solver follows the exposition in Kahan's paper "To solve a real cubic equation".
;;    https://people.eecs.berkeley.edu/~wkahan/Math128/Cubic.pdf

;; A real, cubic equation has the form
;;    A x^3 + B x^2 + C x + D = 0
;; where the coefficients A, B, C, and, D are *real* numbers.

;; Note: Closed form solutions can lead to numerical unstability in the so called
;;       irreducible case: The case of three irrational real roots
;;       leads to trigometric calculations.

;; This solver will use a version of Newton iteration specialized for cubic equations.

;;; Newton Iteration

;;    Q(x) = A x^3 + B x^2 + C x + D
;;    Pick x0 near a root.
;;    x_{n+1} = x_n - Q(x_n)/Q'(x_n)   for n=0,1,...

;; We need to:

;;   1) Calculate Q(x_n)/Q'(x_n) efficiently
;;   2) Find a good starting value of x_0
;;   3) Find a criterium for stopping
;;   4) After finding one zero, find the other two

;; Ad 1)

;;   Q (x) = A x^3 +  B x^2 +  C x + D
;;   Q'(x) =         3A x^2 + 2B x + C

;; Claim:
;;   Put 
;;         q0 = A x
;;         q1 = q0+B
;;         q2 = q1 x + C
;;   Then
;;         Q'(x) = (q0+q1) x + q2
;;         Q (x) = q2 x + D

;;   Proof
;;           (q0+q1)   x + q2
;;         = (q0+q1)   x + q1 x + C
;;         = (q0+q0+B) x + (q0+B) x + C
;;         = (2q0+B) x + (q0+B) x + C
;;         = (2Ax+B) x + (Ax+B) x + C
;;         =  3Ax^2 + 2BX + C
;;         = Q'(X)

;;            q2 x + D
;;         = (q1 x + C) x + D
;;         = ((q0+B) x + C) x + D
;;         = ((Ax+B) x + C) x + D  
;;         = Ax^3 + Bx^2 + Cx + D

;; The proof shows that q0, q1, and, q2 basically is coeffecients from Horner's rule.



;; Ad 2)

;;   The speed of Newton iteration depends on the closeness of the starting point from the root.
;;   The speed is also affected by the multiplicity of the root.
;    At the same time stability during deflation must also be considered.

;;  Assuming AD≠0 :
;;      b := -(B/A)/3
;;      r := |Q(b)/A|^(1/3) >= 0
;;      s := sign( Q(b)/A ) = ±1
;;  If Q'(b)/A >= 0 
;;  then  x0 := b - sr
;;  else  x0 := b - 1.324718 s max(r, sqrt( -Q'(b)/A )

;; Notes: The number b is chosen s.t. Q''(b)=0.
;;        Q''(x) = 6A x + 2B  thus Q''(b) = Q''(-(B/(3A)) = 0

;; Note:  The number λ=1.324718... is the real root of λ^3 = λ + 1.

;; Ad 4) Deflation
;;   Given a root r deflation computes a quadratic polynomium a x^2 + b x +c
;;   such the that the remaining two roots are the roots in the quadratic polynomial.

;;   Let's find expressions for a, b, and, c in terms of A, B, C, D, and, the root r.

;    (x-r)(ax^2 + bx + c)
; =     ax^3 + bx^2 + cx - arx^2 - brx - cr
; =     ax^3 + (b-ar)x^2 +(c-br)x - cr
; Compare with
;       Ax^3 +      Bx^2 +     Cx + D
; Thus:
;       A=a,  B=b-ar,  C=c-br,  D=-cr
;       a=A,  b=B+ar,  c=C+br,  c=D/-r
; Note: We have two different expressions for c.

; The equations work fine if |x^3|>|D/A|,
; but otherwise a different deflation is used in order to avoid numerical instability.

(define (solve-cubic A B C D)
  ; SYNTAX (complex e1 e2)
  ;    Evaluate e1 and e2. Call the results x and y.
  ;    Construct the number x+iy if y≠0,
  ;    and                  x    if y=0
  (define-syntax (complex stx)
    (syntax-case stx () [(_complex x y) (syntax/loc stx (if (= y 0.) x (+ x (* +i y))))]))
  ; Discriminant of a x^2 + b/2 x + c
  (define (discriminant a b c)
    ; TODO: Implement the discriminant in Kahan's paper
    (- (* b b) (* a c))) ; note: b=B/2
  ; Solve the quadratic equation: A x^2 + B x + C =0
  (define (solve-quadratic A B C)
    (define b (/ B -2.))
    (define d (discriminant A b C))
    (cond
      ; complex roots
      [(< d 0.)  (define x1 (/ b A))
                 (define x2 x1)
                 (define y1 (/ (sqrt (- d)) A))
                 (define y2 (- y1))
                 (values x1 y1 x2 y2)]
      ; real roots
      [else      (define y1 0.)
                 (define y2 0.)
                 (define r (+ b (* (flsgn b) (sqrt d))))
                 (cond
                   [(= r 0.) (define x1 (/ C A))
                             (define x2 (- x1))
                             (values x1 y2 x2 y2)]
                   [else     (define x1 (/ C r))
                             (define x2 (/ r A))
                             (values x1 y1 x2 y2)])]))
  ; Compute Q(x) and Q'(x) efficiently
  (define (Q&Q1 x)
    ; Names: Q = Q(x)  and Q1 = Q'(x)
    (define q0 (* A x))
    (define q1 (+ q0 B))
    (define q2 (+ (* q1 x) C))
    (define Q1 (+ (* (+ q0 q1) x) q2))
    (define Q  (+ (* q2 x) D))
    (values Q Q1))
  ; Pick a good initial value from which to start the iteration 
  (define (choose-initial-x)
    (cond
      [(= A 0.0) (error)]
      [(= D 0.0) (error)]
      [else ; neither A nor D is zero
       (define b (/ (/ B A) -3.0))
       (define-values (Q Q1) (Q&Q1 b))
       (define t  (/ Q A))
       (define r  (expt (abs t) (/ 1. 3.)))  ; >= 0
       (define s  (flsgn t))  ; = ±1
       (define t1 (/ Q1 A))
       (if (>=  t1 0.)
           (- b (* s r))
           (- b (* 1.324717957244746 s (max r (sqrt (- t1))))))]))
  ; Find a real root of the cubic equation (all equations have at least one real root)
  (define (find-one-root x)
    (define (iterate x)
      (define-values (Q Q1) (Q&Q1 x))
      (cond
        [(= Q1 0.0) x]
        [else       (define x1 (- x (/ (/ Q Q1) 1.000000000000001)))
                    (if (= x1 x) x1 (iterate x1))]))
    (iterate x))
  ; Given a real root r of the cubic equation,
  ; return coeffecients of a quadrativ polynomial where x-r has
  ; been factored out.
  (define (deflate r)
    (cond
      [(>= (abs (expt r 3)) (abs (/ D A)))
       (define a A)
       (define c (/ (- D) r))
       (define b (/ (- c C) r))
       (values a b c)]
      [else    
       (define a A)
       (define b (+ B (* a r)))
       (define c (+ C (* b r)))
       (values a b c)]))  
  ; Use the quadratic solver in the easy cases
  (cond
    ; Make sure all numbers are inexact
    [(not (and (inexact? A) (inexact? B) (inexact? C) (inexact? D)))
     (solve-cubic (exact->inexact A) (exact->inexact B) (exact->inexact C) (exact->inexact D))]
    ;; Linear equation
    [(and (= A 0) (= B 0))
     (list (/ (- D) C))]
    ;; Got a quadratic equation
    [(= A 0.) (define-values (x1 y1 x2 y2) (solve-quadratic B C D))
              (list (complex x1 y1) (complex x2 y2))]
    ;; Factor out x, then solve quadratic
    [(= D 0.) (define-values (x1 y1 x2 y2) (solve-quadratic A B C))
              (cons 0. (list (complex x1 y1) (complex x2 y2)))]
    ;; The main case
    [else     (define r (find-one-root (choose-initial-x)))
              (define-values (a b c) (deflate r))
              (define-values (x1 y1 x2 y2) (solve-quadratic a b c))
              (list r (complex x1 y1) (complex x2 y2))]))

#;(
;;; These examples are the Kahan's paper
(solve-cubic  1.  -6.  11.  -6.)
(solve-cubic -1.   0.   0.  -1.)
(solve-cubic  1.   3.   3.   1.)
(solve-cubic  1.   0.  -2.  -5.)
(solve-cubic  1.  -3.   2.   0.)
(solve-cubic  1.  -3.   2.   2.34e-89)
(solve-cubic  1.  -7999999999. -8000000002. 16000000000.)
(solve-cubic  16000000000. -8000000002. -7999999999. 1.)
(solve-cubic  1 -99999.00001 -99999.00001 1)
(solve-cubic  0.01 -300 2990000 -299)
(solve-cubic -3 3 -1 0.1111111111)

(solve-cubic 1. -3. 3. -0.9999999999999999)

)

; (solve-cubic 0 0 -4 -1) ; a linear equation