#lang racket
(require "cubic-equation.rkt")

(define (displayln x) (void))

;;;
;;; Conics
;;;

; This library provides functions that can do calculations with conic curves in the plane.
; Conics can be described as the zero-set of a quadratic form:

; A quadratic form :
;    A x^2 + B y^2 + C xy + D x + E y + F

; Introducing homogenous coordinates and embedding (x,y) -> [x,y,1]
; allows a matrix representation of the quadratic form.
; Renaming the coefficients
;     a x^2 + c y^2 + f z^2 + 2b xy + 2d xz + 2e yz 
; this corresponds to the symmetric matrix: 

;            [a b d]  (x)
;   (x,y,z) ·[b c e]· (y)
;            [d e f]  (z)

;;; Circles

; As an example, let's find the matrix representation of a circle.

; Circle with center (X,Y) and radius R:
;      (x - X)^2 + (y - Y)^2 = R^2
; <=>  x^2 -2xX + X^2  +  y^2 -2yY + Y^2 = R^2
; <=>  x^2 + y^2 - R^2 + 0 xy - 2X x - 2Y y = 0
; Introducing homegenous coordinates:
; <=>  x^2 + y^2 - R^2 z^2 + 0 xy - 2X xz - 2Y yz = 0

; So       a=1, c=1, f=-R^2, 2b=0, 2d=-2X, 2e=-2Y
; that is  a=1, c=1, f=-R^2,  b=0,  d=-X,   e=- Y
;
; [ 1  0  -X   ]
; [ 0  1  -Y   ]
; [-X -Y  -R^2 ]


;;;
;;; References
;;;

; Book: "Perspectives on Projective Geometry" by Jürgen Richter-Gebert 
; Code: https://github.com/CindyJS/CindyJS/blob/master/src/js/libgeo/GeoOps.js
; Note: The code is from a CindyJs a JavaScript version of Cinderella.

;;;
;;; Homogeneous Coordinates
;;;

; The set R^2 consisting og all points (x,y) can be embedded in P^3
; sending (x,y) to [x,y,1]. In projective space [x,y,z] = [αx,αy,αz] for all α≠0.
; An element of the form [x,y,0] is outside the image of this embedding.
; The set of elements of the form [x,y,0] is called the "line at infinity".
; For each direction vector (x,y) there is point at infinity [x,y,0].

(struct hom (x y z) #:transparent)

(define (inj x y) ; injection of R^2 into P^3
  (hom x y 1.))

(define (back h) ; the inverse of inj (assumes z≠0)  
  (match h
    [(hom x y 0) (list x y 0)]
    [(hom x y z) (list (/ x z) (/ y z))]))

(define (finite? h)   (match h [(hom _ _ 0.0) #f] [_ #t]))
(define (infinite? h) (match h [(hom _ _ z)   (not (zero? z))]))

; SYNTAX (with-hom [h (a b c)] body ...)
(define-syntax (with-hom stx)
  (syntax-case stx ()
    [(_with-hom [h (a b c)] body ...)
     (syntax/loc stx (match h [(hom a b c) body ...]))]
    [_ (raise-syntax-error 'with-hom "hmm" stx)]))

; SYNTAX (with-homs ([h (a b c)] ...) body ...)
(define-syntax (with-homs stx)
  (syntax-case stx ()
    [(_with-homs ([h (a b c)] ...) body ...)
     (syntax/loc stx (match* (h ...) [((hom a b c) ...) body ...]))]
    [_ (raise-syntax-error 'with-homs "hmm" stx)]))

;;;
;;; Homogenous Operations
;;;

; The following operations are straightforward.
;     ⊗   scale with number t
;     ⊘   scale with number 1/t
;     ⊕   add two homs
(define (⊗ t h) (with-hom [h (x y z)]                (hom (* t x) (* t y) (* t z))))
(define (⊘ h t) (with-hom [h (x y z)]                (hom (/ x t) (/ y t) (/ z t))))
(define (⊕ h k) (with-homs ([h (x y z)] [k (a b c)]) (hom (+ x a) (+ y b) (+ z c))))

;;;
;;; Lines and Points
;;; 

; A lines in R^2 given by  has an equation of the form
;        a x + b y + c   = 0 
; In homogenous coordinates:
;        a x + b y + c z = 0
; Homogenous representationt 
;        [a,b,c]

; As an example the line at infinity as the equation z=0.
(define line-at-infinity (hom 0 0 1))

;;; Incidence and dot products

; A point (X,Y) is on the line given by a x + b y + c = 0
; if and only if                        a X + b Y + c = 0.
; Using the homogenous representation:
; A point [X,Y,Z] = [X/Z,Y/Z,1] is on the line given by a x   + b y   + c z = 0
; <=>                                                   a X/Z + b Y/Z + c 1 = 0
; <=>                                                   a X   + b Y   + c Z = 0.
; <=>                                                   [a,b,x] dot [X,Y,Z] = 0.

; That is if l and p are homogenous representations of a line and a point,
; then p lies on l if only if l·p = 0.

(define (dot p q) ; dot product aka scalar product
  (match* (p q) [((hom x y z) (hom a b c)) (+ (* a x) (* b y) (* c z))]))

(define ε 1e-12)
(define (zeroish? x) (< (magnitude x) ε))

; incident? : point line -> boolean
;   does the point p lie on the line l?
(define (incident? p l)
  (zeroish? (dot p l)))

;;; Intersections of lines, line through points and cross products

; The cross product of vectors in R^3 looks like this:

; cross : hom hom -> hom
(define (cross p q)
  (with-homs ([p (x y z)] [q (a b c)])
    (hom (- (* y c) (* z b))
         (- (* z a) (* x c))
         (- (* x b) (* y a)))))

; In projective space the cross products of two lines (represented as homs)
; will give the point, where the two lines join. In the case where
; the two points, are equal the result is (hom 0 0 0) (a point at infinity).

; join : line line -> point
;   find the intersection of the two lines
(define (join l m) (cross l m))

; In projective space the cross products of two points (represented as homs)
; will give the join of two points (the line through the points).

; meet : point point -> line
;   find the line through the two points
(define (meet p q) (cross p q))

; Example: Page 54
#;(
   (define A (inj 1 1))
   (define B (inj 3 2))
   (define C (inj 3 0))
   (define D (inj 4 1))
   (define l_AB (join A B))
   (define l_CD (join C D))
   (define E    (meet l_AB l_CD))  ; [7,4,1] expected
   )


(define (parallel p l)  ; find line through the point p parallel to the line l
  (join p (meet l line-at-infinity)))

;;;
;;; Matrices (3x3)
;;;

; Transformations on P^3 are represented as 3x3 matrices

(struct mat (a b c d e f g h i) #:transparent)

; Since we often need to bind the elements to variables,
; it is convenient to introduce a little syntax.

; SYNTAX (with-matrix [M (a b c d e f h i)] body ...)
(define-syntax (with-matrix stx)
  (syntax-case stx ()
    [(_with-matrix [M (a b c d e f g h i)] body ...)     
     (syntax/loc stx (match M [(mat a b c d e f g h i) body ...]))]
    [_ (raise-syntax-error 'with-matrix "hmm" stx)]))

; SYNTAX (with-matrices ([M (a b c d e f h i)] ...) body ...)
(define-syntax (with-matrices stx)
  (syntax-case stx ()
    [(_with-matrices ([M (a b c d e f g h i)] ...)
                     body ...)
     (syntax/loc stx
       (match* (M ...)
         [((mat a b c d e f g h i) ...)
          body ...]))]
    [_ (raise-syntax-error 'with-matrices "hmm" stx)]))

; Applying a transformation M on a point p is written M·p.
; Standard formula.

(define (· M p)  ; \cdot
  (with-matrix [M (a b c d e f g h i)]
    (with-hom  [p (x y z)]
      (hom (+ (* a x) (* b y) (* c z))
           (+ (* d x) (* e y) (* f z))
           (+ (* g x) (* h y) (* i z))))))

;;; Constructors of standard transformations

(define (translation tx ty)
  ; translate tx along the x-axis, and
  ;           ty along the y-axis
  (mat 1 0 tx
       0 1 ty
       0 0 1))

(define (rotation α)
  ; rotate α radian in positive direction
  (define c (cos α))
  (define s (sin α))
  (mat    c  (- s) 0 
          s     c  0
          0     0  1))

;;; Operators

(define (transpose M)
  (match M
    [(mat a b c
          d e f
          g h i)
     (mat a d g
          b e h
          c f i)]))


(define (△ M) ; adjoint
  ; (△ M) = M^-1 * det(M)
  ; The adjoint is "almost" the inverse.
  ; Note: The formula in the book assumes M is symmetric.
  ;       This formula is general.
  (with-matrices ([M (a b c d e f g h i)])
    (mat    (- (* e i) (* f h))     (- (- (* b i) (* c h)))    (- (* b f) (* c e))
         (- (- (* d i) (* f g)))       (- (* a i) (* c g))  (- (- (* a f) (* c d)))
            (- (* d h) (* e g))     (- (- (* a h) (* b g)))    (- (* a e) (* b d)))))
     
; Construct 3x3 matrix [v1;v2;v3] from the
; column vectors v1,v2, and, v3.
(define (mat3 v1 v2 v3)
  (with-homs ([v1 (a d g)]
              [v2 (b e h)]
              [v3 (c f i)])
    (mat a b c d e f g h i)))

(define (row M index)
  ; return the row indicated by index
  (with-matrix [M (a b c d e f g h i)]
    (match index
      [0 (hom a b c)]
      [1 (hom d e f)]
      [2 (hom g h i)]
      [_ (error 'row "got: ~a" (~a M " " index))])))

(define (col M index)
  ; return the column indicated by index
  (with-matrix [M (a b c d e f g h i)]
    (match index
      [0 (hom a d g)]
      [1 (hom b e h)]
      [2 (hom c f i)])))

;;;
;;; Determinants
;;; 

; determinant of a 2x2 matrix [a,b ; c,d]
(define (det2 a b c d) (- (* a d) (* b c)))

; Determinant of a 3x3 matrix
(define (det M)
  (with-matrix [M (a b c d e f g h i)]
    (+ (*    a (- (* e i) (* f h)))
       (* -1 b (- (* d i) (* f g)))
       (*    c (- (* d h) (* e g))))))


; Determinant of the 3x3 matrix [v1;v2;v3]
; where v1,v2,v3 are column vectors
(define (det3 v1 v2 v3)
  (det (mat3 v1 v2 v3)))

; Note: Three points p1 p2 p3 are collinear (on the same line)
;       if and only if (det3 p1 p2 p3) = 0

(define (collinear? p1 p2 p3)
  (zeroish? (det3 p1 p2 p3)))

;;;
;;; Cross Ratios
;;;

; Cross ratios are the simplest invariant under projective transformations.

(define (cross-ratio a b c d)
  ; four collinear points a, b, c, and, d all finite.
  ; pick o not on the line
  (define o (hom 1 0 0)) ; at infinity
  (/ (* (det3 o a c) (det3 o b d))
     (* (det3 o a d) (det3 o b c))))

;;;
;;; Quadratic Forms
;;;

; A quadratic form :
;    a x^2 + c y^2 + f z^2 + 2b xy + 2d xz + 2e yz 

;            [a b d]  (x)
;   (x,y,z) ·[b c e]· (y)
;            [d e f]  (z)

; The set of points that fulfill a x^2 + c y^2 + f z^2 + 2b xy + 2d xz + 2e yz = 0
; will be called a conic.

(struct form (a b c d e f) #:transparent)

(define (form->matrix q)
  (match-define (form a b c d e f) q)
  (mat a b d b c e d e f))

(define (matrix->form M)
  (with-matrix [M (a b c d e f g h i)]
    (form a b c d e f)))

(define (form-eval Q x y)
  (match Q
    [(form a b c d e f)
     (+ (* a x x)   (* c y y) f
        (* 2 b x y) (* 2 d x) (* 2 e y))]))

(define (Form x^2 y^2 z^2 xy xz yz)
  (form x^2 (/ xy 2.) y^2 (/ xz 2.) (/ yz 2.) z^2))

; THEOREM
;   IF A is a symmetric 3x3 real matrix with det(A)≠0,
;   and p is a point of the corresponding conic,
;   then A·p is the homogenous coordinates for the tangent at the point p to the conic.

(define (tangent p conic)
  (· (form->matrix conic) p))

(define (degenerate conic)
  (zeroish? (det (form->matrix conic))))

(define (index-of-nonzero-diagonal-entry M)
  ; pick index with entry of greatest magnitude
  (with-matrix [M (a b c d e f g h i)]
    (define     diag (list a e i))
    (define abs-diag (for/list ([x diag] [i 3])
                       (list (magnitude x) i)))
    (define sorted   (sort abs-diag > #:key first))
    (define idx (second (first sorted)))
    (if (zeroish? (first (first sorted)))
        #f     ; no non-zero diagonal
        idx))) ; success

(define (matrix->vector-of-vector M)
  (with-matrix [M (a b c d e f g h i)]
    (vector (vector a b c)
            (vector d e f)
            (vector g h i))))

(define (indices-of-max M)
  ; find the indices of the element with the greatest absolute value
  (define rows (matrix->vector-of-vector M))
  (define-values (_ i+j)
    (for/fold ([m #f] [index #f]) ([row rows] [i 3])
      (for/fold ([m m] [index index]) ([x row] [j 3])
        (if (or (not m) (> (magnitude (real-part x)) (magnitude (real-part m))))
            (values x (list i j))
            (values m index)))))
  (values (first i+j) (second i+j)))

(define (index&value-of-max-abs h)
  (with-hom [h (a b c)]
    (define v (vector a b c))
    (define-values (_ i)
      (for/fold ([m #f] [index #f])
                ([x (in-vector v)] [i (in-range 3)])
        (if (or (not m) (> (magnitude x) (magnitude m)))
            (values x i)
            (values m index))))
    (values i (vector-ref v i))))

(define (mat-ref M s t)
  (with-matrix [M (a b c d e f g h i)]
    (match* (s t)
      [(0 0) a] [(0 1) b] [(0 2) c]
      [(1 0) d] [(1 1) e] [(1 2) f]
      [(2 0) g] [(2 1) h] [(2 2) i]
      [(_ _) (error 'mat-ref "~a" (~a (list M s t)))])))

(define (cols M)
  (with-matrix [M (a b c d e f g h i)]
    (values (hom a d g) (hom b e h) (hom c f i))))

(define (mat+ M N)
  (with-matrices ([M (a b c d e f g h i)]
                  [N (x y z u v w r s t)])
    (mat (+ a x) (+ b y) (+ c z)
         (+ d u) (+ e v) (+ f w)
         (+ g r) (+ h s) (+ i t))))

(define (mat* M constant/matrix/hom)
  (define-syntax (dot stx)
    (syntax-case stx () [(_dot a b c x y z) (syntax/loc stx (+ (* a x) (* b y) (* c z)))]))  
  (match* (M constant/matrix/hom)
    [((? number? k) (mat a b c d e f g h i))
     (mat (* k a) (* k b) (* k c) (* k d) (* k e) (* k f) (* k g) (* k h) (* k i))]
    [((mat a b c d e f g h i) (mat x y z u v w r s t))
     (mat (dot a b c  x u r)  (dot a b c  y v s)  (dot a b c  z w t)
          (dot d e f  x u r)  (dot d e f  y v s)  (dot d e f  z w t)
          (dot g h i  x u r)  (dot g h i  y v s)  (dot g h i  z w t))]
    [((mat a b c d e f g h i) (hom x y z))
     (hom (dot a b c  x y z)  (dot d e f  x y z)  (dot g h i  x y z))]))

(define (mat** M N T)
  (mat* M (mat* N T)))

(define (split-degenerate A) ; matrix
  (displayln (list 'split 'A A))
  (define (M_ p) (with-hom [p (λ μ τ)] (mat 0. τ (- μ) (- τ) 0. λ μ (- λ) 0.)))
  ; Note: M_p·q = p x q
  (define B (△ A))
  (displayln (list 'split 'B B))
  (define k (index-of-nonzero-diagonal-entry B))
  (define (finish C)
    (displayln (list 'split 'C C))
    (define-values (i j) (indices-of-max C))
    (displayln (list 'split-degenerate 'i i 'j j 'non-zero-Cij (mat-ref C i j)))
    (define g (row C i))
    (define h (col C j))
    (displayln (list 'split: 'lines-are: g h))
    ; now g and h are the coordinates of the two lines
    (values g h))
  (cond
    [(or (not k) (zeroish? (mat-ref B k k)))
     ; When B is zero, it means A describes a conic with a double line
     (finish A)]
    [else
     ; Note: The books is missing the minus sign on the next line.     
     (define β (sqrt (- (mat-ref B k k))))
     (displayln (list 'split 'β β))
     (define p (⊘ (col B k) β))
     (define C (mat+ A (M_ p)))
     (finish C)]))

(define (omit M k)
  ; Given the 3x3 matrix M, return the entries of the
  ; 2x2 matrix where the k'th row and the k'th column of M is omitted
  (with-matrix [M (a b c d e f g h i)]
    (match k
      [0 (values e f h i)]
      [1 (values a c g i)]
      [2 (values a b d e)]
      [_ (error)])))

(define (minor M k)
  (if (odd? k)
      (omit (mat* -1 M) k)
      (omit M k)))
  
(define (intersect-conic-and-line A l) ; A matrix
  (define (upper-2x2 M) (match M [(mat a b _ d e _ _ _ _) (values a b d e)]))
  (define (M_ p) (with-hom [p (λ μ τ)] (mat 0. τ (- μ) (- τ) 0. λ μ (- λ) 0.)))  
  (define M_l (M_ l))
  (match-define (hom λ μ τ) l)
  (define B (mat** (transpose M_l) A M_l))
  (displayln (list 'intersection-c&l 'B B))
  ; Note: Find non-zero (we pick the greatest) coordinate of l,
  ;       and use the corresponding minor of B.
  (define-values (s v) (index&value-of-max-abs l))
  (define-values (B11 B12 B21 B22) (omit B s))  ; TODO: CHANGE BACK TO OMIT
  ; 
  (define α (/ (sqrt (- (det2 B11 B12 B21 B22))) v))
  (define C (mat+ B (mat* α M_l)))
  (define-values (i j) (indices-of-max C))
  (displayln (list 'intersect-conic-and-line 'i i 'j j))
  (define p (row C i))
  (define q (col C j))
  (list p q)) ; two points

(define (solve/alternative α β γ δ)
  (displayln (list δ γ β α))
  ;     α λ^3 + β λ^2 μ + γ λμ^2 + δ μ^3 = 0 and λ=1
  ; <=> α     + β     μ + γ  μ^2 + δ μ^3 = 0
  (define (f μ) (+ α (* β μ) (* γ μ μ) (* δ μ μ μ)))
  (define μs (solve-cubic δ γ β α))
  (displayln (list 'solve/alternative α β γ δ))
  (displayln (list 'solve/alternative 'λ 1 'μs μs))
  (displayln (list 'solve/alternative 'all-should-be-zero: (map f μs)))
  (values (list 1. 1. 1.) μs))

(define (different-solutions λs μs)
  ; TODO: this assumes solve/alternative is used (and λ is always 1)
  (match-define (list μ1 μ2 μ3) (sort μs (λ (x y) (< (magnitude x) (magnitude y)))))
  (values 1. μ1 1. μ3))

(define (intersect-conic-and-conic A B) ; matrices A and B represents two conics
  (displayln (list 'A A 'B B))
  (define-values (A1 A2 A3) (cols A))
  (define-values (B1 B2 B3) (cols B))
  (displayln (list 'As A1 A2 A3))
  (displayln (list 'Bs B1 B2 B3))
  ; We want to solve det(A + μB) = 0 which is a cubic equation.
  ; Calculate coefficients.
  (define α (det A))
  (define β (+ (det3 A1 A2 B3) (det3 A1 B2 A3) (det3 B1 A2 A3)))
  (define γ (+ (det3 A1 B2 B3) (det3 B1 A2 B3) (det3 B1 B2 A3)))
  (define δ (det B))
  ;; Find two degenerate conics C1, and C2, such that the
  ;; intersections of A and C matches the intersections of A and B.
  (cond
    ; if only one is degenerate make sure it is A
    [(< (abs δ) (abs α)) (intersect-conic-and-conic B A)] ; swap A and B
    [else     
     (define-values (C1 C2)
       (cond
         ;; If both A and B are degenerate, then pick A and B as C1 and C2
         [(zeroish? δ) (values A B)] ;   α < δ ~ 0 so both A and B are degenerate
         ;; At least B is non-degenerate
         [else ; Examine the pencil generated by A and B to find a degenerate conic
          ; Find λ and μ s.t. C=λA+μB is degenerate
          ; Note that the intersections of A and C are the same as the intersections of A and B.
          (define-values (λs μs) (solve/alternative α β γ δ))
          ; (define-values (λs μs) (solve/alternative δ γ β α))
          (displayln (list 'μs μs))
          (define-values (λ1 μ1 λ2 μ2) (different-solutions λs μs))
          (displayln (list (list 'λ1 λ1 'μ1 μ1) (list 'λ2 λ2 'μ2 μ2)))
          ; Generate two *different* conics
          ; Since B is non-degenerate we get two different conics here
          (define C1 (mat+ (mat* λ1 A) (mat* μ1 B)))
          (define C2 (mat+ (mat* λ2 A) (mat* μ2 B)))
          (values C1 C2)]))
     (displayln (list 'intersect-c&c 'C1 C1 'det-C1 (det C1) "should be zero"))
     (displayln (list 'intersect-c&c 'C2 C2 'det-C2 (det C2) "should be zero"))
     ; Split C1 and C2 into lines 
     (define-values (g1 h1) (split-degenerate C1)) ; g and h are now lines
     (define-values (g2 h2) (split-degenerate C2)) ; g and h are now lines
     (displayln (list 'intersect-c&l: 'the-lines g1 h1)) 
     (displayln (list 'intersect-c&l: 'the-lines g2 h2)) 
     ; Find intersections between the lines and A
     (list  (join g1 g2) (join g1 h2) (join h1 g2) (join h1 h2))
     #;(display
        (cons 'points-using-join
              (map back (list  (join g1 g2) (join g1 h2) (join h1 g2) (join h1 h2)))))
     ;(newline)
     #;(append (intersect-conic-and-line A g1)
             (intersect-conic-and-line A h1)
             (intersect-conic-and-line A g2)
             (intersect-conic-and-line A h2)
             (intersect-conic-and-line B g1)   ; B seems to work - why ?
             (intersect-conic-and-line B h1)
             (intersect-conic-and-line B g2)
             (intersect-conic-and-line B h2))]))

;(intersect-conic-and-line (form->matrix (form 1 1 0 0 0 -1)) (hom 1 1 1))

#; (map back (intersect-conic-and-conic (form->matrix (Form 1. -1. -2. 0. 0. 0.))
                                        (form->matrix (Form .5 -1. -1. 0. 0. 0.))))
#;(map back
       (apply append (intersect-conic-and-conic (form->matrix (Form 1. -1. -2. 0. 0. 0.))
                                                (form->matrix (Form .5 -1. -1. 0. 0. 0.)))))
;(back p1)
;(back p2)
;(back p3)
;(back p4)

(define (pt x y) (inj x y))
(define (line p q) (join p q))
(define (circle x y r)
  (define -x   (- x))
  (define -y   (- y))
  (define -r^2 (- (* r r))) 
  (mat  1  0 -x
        0  1 -y
       -x -y -r^2))
(define (to-conic l)
  ; turn the line into conic (a double line)
  (with-hom [l (a b c)]
    ; a x + by + c z = 0  <=> (a x + by + c z)^2 = 0
    ; a^2 x^2 + 2 ab xy + 2ac xz + b^2 y^2 + 2bc yz + c^2 z^2
    (mat (* a a) (* a b) (* a c)
         (* a b) (* b b) (* b c)
         (* a c) (* b c) (* c c))))

(define (two-lines l m)
  (with-homs ([l (a b c)]
              [m (d e f)])    
    ; a x + by + c z = 0  and  d x + e y + f z = 0 
    ; (a x + by + c z)(d x + e y + f z) =0
    ; ad x^2 + ae xy + bd xy + af xz + cd xz + be y^2 + bf yz + ce yz + cf z^2 = 0
    (mat (*     a d)                 (+ (* 0.5 a e) (* 0.5 b d)) (+ (* 0.5 a f) (* 0.5 d d))
         (+ (* 0.5 a e) (* 0.5 b d)) (*     b e)                 (+ (* 0.5 b f) (* 0.5 c e))
         (+ (* 0.5 a f) (* 0.5 d d)) (+ (* 0.5 b f) (* 0.5 c e)) (*     c f))))

'Test
; Intersect the unit circle with the line y=0
(map back (intersect-conic-and-line (circle 0 0 1) (line (pt 0 0) (pt 2 0)))) ; (1,0) (-1,0) 
(map back (intersect-conic-and-line (circle 0 0 1) (line (pt 0 0) (pt 4 0)))) ; (1,0) (-1,0)
(map back (intersect-conic-and-line (circle 0 0 1) (line (pt 0 0) (pt 6 0)))) ; (1,0) (-1,0)
; (intersect-conic-and-conic (circle 0 0 1) (to-conic (line (pt 0 0) (pt 2 0))))
;(map back (intersect-conic-and-line (circle 0 0 1) (line (pt 0 0) (pt 0 2)))) ; (0,1) (0,-1)

;(to-conic (line (pt 0 0) (pt 2 0))) ; double line
;(split-degenerate (to-conic (line (pt 0 0) (pt 2 0))))

(map back (intersect-conic-and-line  (circle 0 0 1) (hom 0 4 0)))
(map back (intersect-conic-and-conic (circle 0 0 1) (to-conic (line (pt 0 0) (pt 0 2))))) ; (0,±1)
(map back (intersect-conic-and-conic (circle 0 0 1) (to-conic (line (pt 0 0) (pt 2 0))))) ; (±1,0)
(map back (intersect-conic-and-conic (circle 0 0 1) (to-conic (line (pt 0 0) (pt 2 2))))) ; (±√2,±√2)
(displayln "---------------------")
(let ()
  ; http://math.stackexchange.com/questions/316849/intersection-of-conics-using-matrix-representation
  (define c1 (form->matrix (Form  1 -1 -2 0 0 0))) ;     x^2 - y^2 - 2 = 0
  (define c2 (form->matrix (Form .5 -1 -1 0 0 0))) ; 0.5 x^2 - y^2 - 1 = 0
  (map back (intersect-conic-and-conic c1 c2)))

(let ()
  ; http://math.stackexchange.com/questions/316849/intersection-of-conics-using-matrix-representation
  (define c1 (form->matrix (Form .5 -1  1 .1 0 0))) ;     
  (define c2 (form->matrix (Form -1  1  1 0 0 0)))  ;     
  (map back (intersect-conic-and-conic c1 c2)))

(split-degenerate (to-conic (line (pt 0 0) (pt 1 3))))

;> (split-degenerate (two-lines (line (pt 0 0) (pt 1 0))   ; y=0
;                               (line (pt 0 0) (pt 0 1)))) ; x=0
;(hom 0.0 -1.0 0) ; y=0
;(hom -1.0 0.0 0) ; x=0
;> (split-degenerate (two-lines (line (pt 0 2) (pt 1 2))    ; y=2
;                               (line (pt 3 0) (pt 3 1))))  ; x=3
;(hom 0.0 -0.0 -6.0)  ; -6=0 ?
;(hom 0.0 0.0 -6.0)   ; -6=0 ?

(let ()
  ; http://math.stackexchange.com/questions/316849/intersection-of-conics-using-matrix-representation
  (define c1 (form->matrix (Form  1 0   0 0 0 -1))) ;     
  (define c2 (form->matrix (Form  3 0  -2 0 0 -1)))  ;     
  (map back (intersect-conic-and-conic c1 c2)))