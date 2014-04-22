#lang racket/base
(module+ test (require rackunit))

;;; Fix: (curve (pt 0 0) .. (pt 0 0) .. cycle)
;;;      Note: (curve (pt 0 0) .. (pt 0 0) .. (pt 0 0) .. cycle) works

;;; Curves

; A curve is represented as a list of Bezier curves and
; a flag that indicates whether the curve is closed or not.
; For two consecutive Bezier curves, the endpoint of the first and the start point
; of the second are equal.

; (struct curve: (closed? bezs) #:transparent #:reflection-name 'curve)

; The user is not expected to use curve: directly. He is to use 
; the constructor  curve. The  curve  constructor takes a description of the curve
; in the form of a "path description".  This description is passed along to
; resolve-path, which computes the list of Bezier curves.

; A match-expander  curve  is also exported, s.t. one can use (curve closed? bezs)
; as a pattern in a match clause.

(require math/matrix racket/match racket/list racket/math racket/function
         (for-syntax racket/base)
         "structs.rkt" "path.rkt" "def.rkt" "pt-vec.rkt" "angles.rkt" "bez.rkt")

(provide 
 ; high level constructors
 curve                        ; construct curve from a path description
 curve*                       ; (λ (ps) (apply curve ps))
 empty-curve                  ; the empty curve
 ; curve operations
 curve-length                 ; return number of bez in the curve
 point-of                     ; given time t (between 0 and the curve length) compute point on curve
 start-point                  ; curve begins here 
 end-point                    ; curve ends here
 curve-reverse                ; return the curve with time reversed
 curve-append                 ; concatenate two curves; the end and start points must match
 intersection-point-and-times ; compute a "first" intersection point and corresponding times
 intersection-point           ; compute "first" intersection point
 intersection-times           ; compute times for "first" intersection point
 intersection-points          ; compute all intersection points
 subcurve                     ; curve between two times; reverse orientation if t2<t1
 cut-before        ; cut the part of c1 that lie before the "first" intersection point of c1 and c2
 cut-after         ; cut the part of c1 that lie after  the "last"  intersection point of c1 and c2
 post-control                 ; see MetaFont Book p.134
 pre-control                  ; see MetaFont Book p.134
 direction-of                 ; velocity vector for curve at a given time
 cyclic?                      ; is the curve closed?
 intercurve                   ; see MetaFont Book p. 134 (there named interpath)
 ; low level constructors (for debug)
 make-choices
 resolve-path-to-bezs
 segment->bezs
 segments->bezs)


(define empty-curve (curve: #f '())) ; the empty curve

(define (make-curve . xs)
  (match xs
    [(list (? pt? p))   (curve: #t (list (bez p p p p)))]
    [(list (? path? p)) (resolve-path p)]
    [(list* path-elms)  (resolve-path (path* path-elms))]))

(define-match-expander curve 
  (λ(stx) (syntax-case stx () [(_ e ...) (syntax/loc stx (curve: e ...))]))
  (λ(stx) (syntax-case stx () [(_ e ...) (syntax/loc stx (make-curve e ...))])))
(define (curve* . xs) (apply make-curve xs))

(define (point-of c t) ; MetaPost "point of" operator
  (defm (curve closed? bezs) c)
  (def n (length bezs))
  (def (point-of-open)
    (cond 
      [(<  t 0) (point-of-bez (first bezs) 0)]
      [(>= t n) (point-of-bez (last bezs) 1)]
      [else (def m (exact-floor t))
            (point-of-bez (list-ref bezs m) (- t m))]))
  (def (point-of-closed)
    (cond 
      [(<  t 0) (point-of (+ t n) c)]
      [(>= t n) (point-of (- t n) c)]
      [else     (def m (exact-floor t))
                (point-of-bez (list-ref bezs m) (- t m))]))
  (cond 
    [closed? (point-of-closed)]
    [else    (point-of-open)]))

(define (start-point c) (point-of c 0))
(define (end-point c)   (point-of c (curve-length c)))

(define (curve-reverse c)
  (defm (curve c? bezs) c)
  (curve: c? (map bez-reverse (reverse bezs))))

(define (curve-append c . cs)
  (curve-append* c cs))

(define (curve-append* c cs)
  (cond [(empty? cs) c]
        [else (curve-append* (curve-append2 c (first cs))
                             (rest cs))]))

(define (curve-append2 c0 c1)
  (defm (curve ?0 bs0) c0)
  (defm (curve ?1 bs1) c1)
  (curve: #f (append bs0 bs1)))   ; todo : look up details of curve appending in Metafont book

(define (subcurve c t0 t1) ; See MetaFontBook p. 133
  (defm (curve cycle? bezs) c)
  (def n (length bezs))   ; 0<=t<=n
  (cond
    [(> t0 t1) (curve-reverse (subcurve c t1 t0))]
    [(= t0 t1) (def p (point-of c t0))
               (curve: #f (list (bez p p p p)))]
    [(< t0 0)  (if cycle? 
                   (subcurve c (+ t0 n) (+ t1 n))
                   (subcurve c 0 (max t1 0)))]
    [(> t1 n)  (cond [cycle? (cond [(>= t0 n) (subcurve c (- t0 n) (- t1 n))]
                                   [else      (error 'subcurve "todo: implement case t0<n<t1")])]
                     [else   (subcurve c (min t0 n) n)])]
    ; 0<=t0<t1<=n :
    [(>= t0 1)  (subcurve (curve: #f (rest bezs)) (- t0 1) (- t1 1))]
    ; 0<=t0<1 :
    [(> t1 1)  (curve-append (subcurve c t0 1) (subcurve c 1 t1))]
    ; 0<=t0<t1<=1 :
    [else (curve: #f (list (bez-subpath (first bezs) t0 t1)))]))

(define (post-control c t) ; See MetaFontBook p. 134
  (defm (curve cycle? bezs) c)
  (def n (length bezs))
  (cond [(< t n) (defm (curve _ (list (bez _ p1 _ _) _ ...)) (subcurve c t n))
                 p1]
        [else    (defm (bez _ _ _ p3) (last bezs))
                 p3]))

(define (pre-control c t) ; See MetaFontBook p. 134
  (defm (curve cycle? bezs) c)
  (def n (length bezs))
  (cond [(> t 0) (defm (curve _ (list _ ... (bez _ _ p2 _))) (subcurve c 0 t))
                 p2]
        [else    (defm (bez p0 _ _ _) (first bezs))
                 p0]))

(define (direction-of c t) ; See MetaFont Book p. 135
  (pt- (post-control c t)
       (pre-control c t)))

(define (curve-length c)
  (defm (curve: _ bs) c)
  (length bs))

(define (cyclic? c)
  (defm (curve: c? _) c)
  c?)

(define (intercurve α c1 c2)  ; !!! the paths must have the same length to get expected result ...
  ; Interpolate between two curves. (Called interpath in the MetaFontBook p. 134)
  (curve: (cyclic? c1) ; assume that the curves are of the same type
          (for/list ([t (in-range (curve-length c1))])
            (def t+1 (+ t 1))
            (bez (med α (point-of     c1 t)   (point-of     c2 t))
                 (med α (post-control c1 t)   (post-control c2 t))
                 (med α (pre-control  c1 t+1) (pre-control  c2 t+1))
                 (med α (point-of     c1 t+1) (point-of     c2 t+1))))))

(define (intersection-times c1 c2)
  ; return times t and u such that c1(t)=c2(u)
  (def ptu (intersection-point-and-times c1 c2))
  (match ptu
    [(list _ t u) (values t u)]
    [#f           (values #f #f)]))

(define (intersection-point c1 c2)
  ; return #f or an intersection point of c1 and c2
  (def ptu (intersection-point-and-times c1 c2))
  (first ptu))

(define (intersection-point-and-times c1 c2)
  ; return either #f or (list p t u), where p = c1(t) = c2(u)
  (defm (curve _ bs1) c1)
  (defm (curve _ bs2) c2)
  (for/or ([b2 (in-list bs2)] [k (in-naturals)])
    (for/or ([b1 (in-list bs1)] [l (in-naturals)])
      (def ptu (bez-intersection-point-and-times b1 b2))
      (and ptu (let () (defm (list p t u) ptu)
                 (list p (+ l t) (+ k u)))))))

(define (intersection-points c1 c2)
  (def ε 0.000001)
  (def n1 (curve-length c1))
  (match (intersection-point-and-times c1 c2)
    [#f '()]
    [(list p t u) 
     (def pre1  (subcurve c1 0 (- t ε)))
     (def post1 (subcurve c1 (+ t ε) n1))
     (append (intersection-points  pre1 c2)
             (list p)
             (intersection-points  post1 c2))]))

(define (cut-before c1 c2)
  ; cuts the parts of c1 that lie before the "first" intersection point of c1 and c2
  ; if there are no intersection point, then nothing is cut from c1
  (defv (t u) (intersection-times c1 c2))
  (or (and (not t) c1)
      (subcurve c1 t (curve-length c1))))

(define (cut-after c1 c2)
  ; cuts the parts of c1 that lie after the "last" intersection point of c1 and c2
  ; if there are no intersection point, then nothing is cut from c1
  (def n (curve-length c1))
  (defv (t u) (intersection-times (curve-reverse c1) c2))
  (or (and (not t) c1)
      (subcurve c1 0 (- n t))))


;;; Break down the process of determining control points of a path.

(define (join-equal-consecutive-knots ks) ; 291
  ; The list ks consists of knots (knot p p- p+ lt rt).
  ; The curve goes through the point p.
  ; If two consecutive points p and q are equal then the "after control point" p+ must be q.
  ; Since p+ now is known, the right type rt, is now explicit.
  ; If both lt and rt are open, then lt becomes (tenscurl 1 1) [that is default tension and curl],
  ; otherwise the lt is kept unchanged.
  ; The same reasoning implies that the "before control point" q- must be p.
  ; The exception is, if the rt is explicit (then p+ is known)
  ; or rt is the end point (then p+) is irrelevant.
  (def loop join-equal-consecutive-knots)
  (match ks
    [(list)  '()]
    [(list k) ks]
    [(list-rest k k+ kmore)
     (defm (knot p p- p+  lt  rt) k)
     (defm (knot q q- q+ qlt qrt) k+)
     (cond [(and (pt~ p q) (not (explicit? rt)) (not (endpoint? rt)))
            (def new-k  (knot p p- p (if (open? lt) (tenscurl 1 1) lt) (explicit)))
            (def new-k+ (knot q p  q+ (explicit) (if (open? qrt) (tenscurl 1 1) qrt)))
            (cons new-k (loop (cons new-k+ kmore)))]
           [(cons k (loop (cons k+ kmore)))])]))

(module+ test
  (let () (defv (p1 p2 p3) (values (pt 0 1) (pt 2 3) (pt 4 5)))
    (def o (open 1)) (def e (explicit)) (def t (tenscurl 1 1))
    (defv (k1 k2) (values (knot p1 #f #f o o) (knot p2 #f #f o o))) (def k3 k1)
    (check-equal? (join-equal-consecutive-knots '())          '())
    (check-equal? (join-equal-consecutive-knots (list k1))    (list k1))
    (check-equal? (join-equal-consecutive-knots (list k1 k2)) (list k1 k2))
    (check-equal? (join-equal-consecutive-knots (list k1 k3)) 
                  (list (knot p1 #f p1 t e) (knot p1 p1 #f e t)))
    (check-equal? (join-equal-consecutive-knots
                   (list (knot (pt 0 0) #f #f (endpoint) (tenscurl 1 1))
                         (knot (pt 0 0) #f #f (open 1)   (open 1))
                         (knot (pt 0 0) #f #f (tenscurl 1 1) (endpoint))))
                  (list
                   (knot (pt 0 0) #f       (pt 0 0) (endpoint) (explicit))
                   (knot (pt 0 0) (pt 0 0) (pt 0 0) (explicit) (explicit))
                   (knot (pt 0 0) (pt 0 0) #f       (explicit) (endpoint))))))

;;;
;;; Construction of curve from path (curve) decription
;;;

; A path is split into segments. The segments begin and end with a "breakpoint" knot.
; This means that the left and right angle both are predescribed.
; That is left-type and right-type aren't both open.

(define (split-into-long-segments p)
  ; A path description p consists of a list of knots.
  ; The path is broken into segments at "break point" knots.
  ; Paths can not be broken at knots whose left and right directions are both open.
  ; At a "break point" knot the left or the right directions is not open.
  ; (displayln (list 'split-... p))
  (define (breakpoint? k) (not (open? (knot-right-type k))))
  (define (not-breakpoint? k) (not (breakpoint? k)))
  (define (no-breakpoints? ks) (not (ormap breakpoint? knots)))
  (define (break-cycle ks)
    (defm (cons (knot p p- p+ lt rt) k..) ks)
    (cons (knot p p- p+ (end-cycle) rt) k..))
  (define (move-breakpoint-to-front ks)
    (defv (nbps more) (splitf-at ks not-breakpoint?))
    (append more nbps))
  (def knots (path-knots p))
  (cond [(no-breakpoints? knots)               ; all open => cycle exists 
         (list (segment (break-cycle knots)))] ; break it!
        [else (let loop ([ks (move-breakpoint-to-front knots)] [segments '()])
                ; (displayln (list (length segments) "ks:"))
                ; (displayln ks)
                (match ks 
                  [(or '() (list _))           ; think: single knot segment? ???
                   (reverse segments)]
                  [(cons k0 k..)     
                   (defv (nbs ks*) (splitf-at k.. not-breakpoint?))
                   (def s (cond [(empty? ks*) (segment knots)]
                                [else         (segment (append (list k0) nbs (list (first ks*))))]))
                   (loop ks* (cons s segments))]))]))

(define (add-info seg closed?)
  ; - add turning angle ψ and distances d- and d+ to the knot
  ; - adjust the left  type (given a) to (given θ)
  ; - adjust the right type (given a) to (given φ)
  ; Note: The given angle is absolute, while θ and φ are relative (and in radian)  
  (defm (segment (list k0* k1* _ ...)) seg)
  (defm (segment (list _ ... kn-* kn)) seg)
  (define (fix k)
    ; If a cycle is broken, end-cycle has been insert into
    ; the left-type of k0. Therefor we need to use kn instead.
    (defm (or (knot _ _ _ lt _) lt) k)
    (if (and lt (end-cycle? lt)) kn k))
  (defv (k0 k1 kn-) (values (fix k0*) (fix k1*) (fix kn-*)))
  (segment
   (for/list ([(k-?* k* k+?*) seg])
     (defv (k-? k k+?) (values (fix k-?*) (fix k*) (fix k+?*)))
     (def k- (or k-? (and closed? (and (equal? k0 kn) kn-))))
     (def k+ (or k+? (and closed? (and (equal? k0 kn) k1))))
     (def dummy (knot #f #f #f #f #f))
     (defm (knot p p- p+  lt  rt) (or k- dummy))
     (defm (knot q q- q+ qlt qrt)     k)
     (defm (knot r r- r+ rlt rrt) (or k+ dummy))
     (def ψ  (or (and k- k+ (turning-angle p q r)) 0))
     (def d- (and k- (dist p q)))
     (def d+ (and k+ (dist q r)))
     (def pq (and p (pt- q p)))
     (def qr (and r (pt- r q)))
     (def qlt* (and k- (match qlt
                         [(given τ a) (given τ (reduce-angle/rad (- (angle pq) a)))]
                         [_ #f])))
     (def qrt* (and k+ (match qrt
                         [(given τ a) (given τ (reduce-angle/rad (- a (angle qr))))]
                         [_ #f])))
     (knot/info q q- q+ (or qlt* qlt) (or qrt* qrt) ψ d- d+))))


; A θ-  +  (B+C) θ  + D θ+  =  -B ψ   - D ψ+     ; k=1..n-1
; A = α-/(β^2 d)    B = (3-α-)/(β^2 d)  C = (3-β)/(α d+)  D = β+/(α^2 d+)
(define (equations seg closed?)
  (defm (segment knots) seg)
  ; at least 3 knots for open paths
  ; at least 2+1 knots for closed paths
  (def n (length knots))
  ;; First equation
  (define (first-equation) ; 298.
    (defm (list k0 k1 _ ...) knots)
    (defm (knot/info p _  p+ _   rt ψ0  _ d+) k0)
    (defm (knot/info q _  _  qlt _  ψ1  _ _)  k1)
    (match rt ; non-cyclic
      [(given τ0 θ0)    (list 0 1 0 θ0)]
      [(explicit)       (def θ0 (signed-angle (pt- p+ p)))
                        (list 0 1 0 θ0)]
      [(tenscurl τ0 γ0) 
       (def τ1 (get-tension qlt))
       (def α0 τ0)
       (def β1 τ1)
       (def χ0 (/ (* (sqr α0) γ0) (sqr β1)))
       (def C0 (+ (*      α0  χ0) 3 (- β1)))
       (def D0 (+ (* (- 3 α0) χ0)      β1))
       
       (def R0 (* -1 D0 ψ1))
       ; (displayln (list 'first: 'χ0 χ0 'α0 α0 'β1 β1 'ψ1 ψ1 'γ0 γ0))
       (list 0 C0 D0 R0)]))
  (define (middle-equations ks)
    (match ks
      [(or '() (list _) (list _ _)) 
       '()]
      [_
       (defm (list k- k k+ _ ...) ks)
       (defm (knot      p p- p+ plt prt)         k-)
       (defm (knot/info q q- q+ qlt qrt ψ d- d+) k)
       (defm (knot/info r r- r+ rlt rrt ψ+ _ _)  k+)
       (def α- (/ 1 (get-tension prt)))
       (def α  (/ 1 (get-tension qrt)))
       (def β  (/ 1 (get-tension qlt)))
       (def β+ (/ 1 (get-tension rlt)))
       (def A (/ α- (* (sqr β) d-)))
       (def B (/ (- 3 α-) (* (sqr β) d-)))
       (def C (/ (- 3 β+)  (* α d+)))
       (def D (/ β+ (* (sqr α) d+)))
       (def R (/ (- (+ (* B ψ) (* D ψ+))) 1))
       ; (displayln (list 'α- α- 'β β 'β+ β+ 'd- d- 'd d+ 'ψ ψ 'ψ+ ψ+))
       ; (displayln (list (list 'A 'B 'C 'D 'R) (list A B C D R)))
       (cons (list A (+ B C) D R)
             (middle-equations (rest ks)))]))
  (define (last-equation) ; 298.
    ; C.append(0)
    ; D.append(0)
    ; curl = path.curl_end
    ; beta_n = path[n].beta
    ; alpha_n_1 = path[n-1].alpha
    ; xi_n = (beta_n**2) * curl / (alpha_n_1**2)
    ; A.append((3-beta_n)*xi_n + alpha_n_1)
    ; B.append(beta_n*xi_n + 3 - alpha_n_1)
    ; R.append(0)
    (defm (list k ... kn- kn) knots)
    (defm (knot p p- p+ plt prt) kn-)
    (defm (knot/info q _  _  qlt _  ψ1  _ _) kn)
    (match qlt ; non-cyclic
      [(given τ φn)     (def θn (- φn)) ; 297.
                        (list 0 1 0 θn)]
      [(explicit)       (def θ (signed-angle2 p p+))
                        #;(def θ (match (signed-angle2 (pt- p+ p) (pt- q p)) ; todo
                                   [+nan.0 0] [θ θ]))
                        ; (displayln (list 'last-equation θ))
                        (error)
                        ; (def θ (signed-angle (pt- p+ p))) ; is this correct?
                        (list 0 1 0 θ)]
      [(tenscurl τ γ)
       (def τ- (get-tension prt))
       (def α τ)
       (def β τ-)
       (def χ (/ (* (sqr β) γ) (sqr α)))
       (def A (+ (* (- 3 β) χ) α))
       (def B (+ (* χ β) 3 (- α)))
       (list A B 0 0)])) ; 298. C=0, D=0
  (defm (list k0 _ ... kn) knots)
  (cond 
    [(and closed? (or (equal? k0 kn) (end-cycle? (knot-left-type k0))))
     (defm (list k0 ks ... kn- kn) knots)
     (defm (list _ k1 _ ...) knots)
     ; We use kn rather than k0 here. The left-type of k0 may have been replaced with end-cycle.
     (def all (middle-equations `(,kn- ,kn ,@ks ,kn- ,kn ,k1)))
     ; (displayln (list "Antal knuder: " (length knots) "Antal ligninger: " (length all)))
     ; (displayln all)
     all] 
    [else 
     (def middle (middle-equations knots))
     (def all `(,(first-equation) ,@middle ,(last-equation)))
     ; (displayln (list "Antal knuder: " (length knots) "Antal ligninger: " (length all)))
     ; (displayln all)
     all]))


(define (solve-long-segment seg closed?)
  ; TODO: Solve the triband system directly without
  ;       constructing a quadratic matrix first.
  (defm (segment knots) seg)
  (defm (list k0 _ ... kn) knots)
  (def eqs (equations seg closed?))
  (define n (length eqs))
  (def eqs/0 ; create the triband matrix by pre- and appending zeros
    (for/list ([eq (in-list eqs)] [i (in-naturals)])
      (drop-right (drop (append (make-list i 0) (drop-right eq 1) (make-list (- n i 1) 0)) 1) 1)))
  ; (displayln (list 'solve k0 kn closed?))
  (def eqs/0* ; if closed fix the corners
    (cond [(and closed? (equal? k0 kn))
           (def eqs/0- (map (λ(e) (drop-right e 1)) eqs/0))
           (def zn (first (first eqs)))
           (defm (list (list z ... _) x ... (list _ y ...) _) eqs/0-)
           `((,@z ,zn)  ,@x (,zn ,@y))] 
          [else eqs/0]))        
  (def M   (list*->matrix eqs/0*))
  (def R*  (for/list ([row eqs]) (list-ref row 3)))
  (def R** (if (and closed? (equal? k0 kn)) (drop-right R* 1) R*))
  (def R   (list->matrix (length R**) 1 R**))
  ; (displayln (list 'eqs eqs 'eqs/0 eqs/0))
  ; (displayln (list 'solve 'eqs/0* eqs/0* 'M M 'R R 'invertible (matrix-invertible? M)))
  (if (matrix-invertible? M)
      (matrix->list (matrix-solve M R))
      (make-list n 0))) ; TODO: avoid this...  Solve point-point directly.

(define (fill-in-angles seg closed?)
  ; Given a segment of knots with info, fill-in-angle will call solve-long-segment to compute θs. 
  ; The θs and the corresponding φs will replace the current left and right types (lt and rt). 
  (def θs (solve-long-segment seg closed?))  ; remove θ[-1]
  (def dummy (λ(_)(knot/info #f #f #f #f #f #f #f #f)))
  (defm (segment (list k0 k1 _ ...)) seg)
  (defm (segment (list _ ... kn- kn)) seg)
  ; (displayln (list "filling angles:" θs))
  ; (displayln (list "filling angles:" (map deg θs)))
  (def filled
    (for/list ([(k-? k k+?) seg] [θ θs])
      (def k- (or k-? (and closed? kn-)))
      (def k+ (or k+? (and closed? k1)))
      (defm (knot/info q q- q+  lt  rt ψ  d-  d+) k)
      (defv (lt* rt*)
        (match* (k- k k+)
          [(#f k0 k+)          (values (endpoint)     (given (get-tension rt) θ))] ; first
          [(k- kn #f) 
           (def φ (- (+ θ ψ))) (values (given (get-tension lt) φ) (endpoint))] ; last
          [(k- k  k+) 
           (def φ (- (+ θ ψ))) (values (given (get-tension lt) φ) (given (get-tension rt) θ))]))
      (knot/info q q- q+ lt* rt* ψ d- d+)))
  ; (displayln filled)
  (segment
   (cond [(and closed? (equal? k0 kn))
          (append filled (list (first filled)))]
         [else    filled])))

(define (fill-in-control-points s closed?)
  ; todo: consider 302. to handle negative tensions
  ; todo: what about 303?
  (defm (segment knots) s)
  (defm (list k0 _ ... kn) knots)
  (def filled
    (let loop ([ks knots])     
      (match ks
        [(or '() (list _)) (0)]
        [(list (knot/info p p- _  lt          (given τ0 θ)  ψ  d-  d+)
               (knot/info q _ q+ (given τ3 φ) qrt   qψ qd- qd+))
         (defv (p+ q-) (control-points p q θ φ τ0 τ3))
         (list (knot/info p p- p+  lt          (given τ0 θ)  ψ  d-  d+)
               (knot/info q q- q+ (given τ3 φ) qrt   qψ qd- qd+))]
        [(list k k+ k.. ...)
         (defm (knot/info p p- _  lt  (given τ0 θ)  ψ  d-  d+) k)
         (defm (knot/info q _ q+ (given τ3 φ) qrt qψ qd- qd+) k+)
         (defv (p+ q-) (control-points p q θ φ τ0 τ3))
         (cons (knot/info p p- p+ lt (given τ0 θ) ψ d- d+)
               (loop (cons (knot/info q q- q+ (given τ3 φ) qrt qψ qd- qd+) k..)))])))
  (segment filled))

(define (make-choices p1)
  (defm (path ks) p1)
  (def p ((if (closed-path? p1) closed-path open-path)
          (join-equal-consecutive-knots ks)))
  (def c (closed-path? p))
  (for/list ([s (split-into-long-segments p)])
    (match s
      ; segments with equal consecutive knots are handled here
      [(segment (list (knot (and p (? pt?)) _ (? pt?) _ _) (knot p (? pt?) _ _ _)))
       s]
      ; now it is safe to call add-info (which computes turning angles)
      [_ (def seg/info (add-info s c))
         ; todo: remove open at endpoints see 303.
         (def seg/angles (fill-in-angles seg/info c))
         (fill-in-control-points seg/angles c)])))

(define (segments->bezs segs) (map segment->bezs segs))

; segment->bezs : segment -> (listof bez?)
;   given a segment (containing a list of knots where the first and last 
;   knot are break points and where the knots between are with info),
;   convert the knots (with control points) into a list of Bezier curves.
(define (segment->bezs seg)
  (defm (segment knots) seg)
  (let loop ([ks knots])
    (match ks
      [(or '() (list _)) '()]
      [(list k k+ k.. ...)
       (defm (knot p _ p+ _ _) k)
       (defm (knot q q- _ _ _) k+)
       (cons (bez p p+ q- q)
             (loop (rest ks)))])))

(define (resolve-path-to-bezs p) (flatten (segments->bezs (make-choices p))))

(define (resolve-path p)
  (def bezs (flatten (segments->bezs (make-choices p))))
  (match p
    [(? open-path?)   (curve: #f bezs)]
    [(? closed-path?) (curve: #t bezs)]))

; two-knots->bez : knot knot -> bez
;   
(define (two-knots->bez k0 k1)
  (defm (knot p _ p+ _ (given τ0 θ)) k0)
  (defm (knot q q- _ (given τ3 φ) _) k1)
  (bez/dirs+tensions p p+ q- q τ0 τ3))

;;;
;;; Transformation
;;;

(define (transform-path t p f) ; internal
  (define (transform-segment seg closed?)
    (defm (segment knots) seg)
    (add-info (segment (map transform-knot knots)) closed?))
  (define (transform-knot k)
    (defm (knot/info p p- p+ lt rt ψ d- d+) k)
    (knot (f p) (f p-) (f p+) lt rt))
  (match p
    [(open-path segments) 
     (add-info (open-path   (map (curryr transform-segment #f) segments)))]
    [(closed-path segments)
     (add-info (closed-path (map (curryr transform-segment #t) segments)))]))

(define (path->bezs p)
  (flatten (segments->bezs (make-choices p))))
