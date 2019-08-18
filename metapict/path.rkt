#lang racket/base
; TODO: TOP PRIORITY
;  Handle direction after last point.

; TODO: As an direction specifier (vec 0 0) counts as #f. Sigh.
;       Handle this in ... somewhere. 

; TODO: (curve (pt@ 1 0) (dir  90) .. (pt@ 1 (rad  90)) (dir 180))
;       The problem is the vec *after* the last point.
(module+ test (require rackunit))

;;;
;;; Paths
;;;

;; A path is basically a list of knots, which can be open or closed.
;; This representation is meant to be internal. Users should use path: to construct paths.
;;    (struct path (knots) #:transparent)

;; A knot consists of 
;;    p                       a point
;;    p-,p+                   the previous and the following control point
;;    left-type, right-type   control info
;; Note: a #f in p- and p+ means the control points haven't been computed yet.
;;    (struct knot (p p- p+ left-type right-type) #:transparent)

;; See MetaPost the program for more information.

#;(define path-example-274 ; 274
    (let () ; This example from section 274 shows how path descriptions are represented.
      (def d (angle (vec -1 -2)))
      (open-path 
       (list (knot (pt 10 20) #f #f           (endpoint)     (tenscurl 1 1))
             (knot (pt 11 21) #f #f           (open 1)       (open -1))
             (knot (pt 12 22) #f #f           (tenscurl 1 2) (tenscurl 1 2))
             (knot (pt 13 23) #f #f           (given 1 d)    (given 3 d))
             (knot (pt 14 24) #f (pt 145 245) (open 4)       (explicit))
             (knot (pt 15 25) (pt 154 254) #f (explicit)     (endpoint))))))

(provide 
 tension                            
 controls
 tension-at-least
 direction-specifier?
 ..                                 ; tension 1 and 1
 ...                                ; tension at least 1 (currently not supported...)
 --                                 ; tension 4095.99998   (value for "infinity" in MetaPost)
 ---                                ; {curl 1} .. {curl  1}
 cycle                              ; 
 path* 
 path:
 display-path)

(require "def.rkt" "structs.rkt" "angles.rkt" "pt-vec.rkt"
         racket/format racket/match racket/list)

(define tension
  (case-lambda
    [(τ)     (unless (>= τ 3/4) (error 'tension "expected a positve tension of 3/4 or greater"))
             (tension-and τ τ)]
    [(τ- τ+) (unless (>= τ- 3/4) (>= τ+ 3/4) 
               (error 'tension "expected a positve tension of 3/4 or greater"))
             (tension-and τ- τ+)]))

(module+ test
  (check-equal? (tension 3) (tension 3 3))
  (check-equal? (tension 3) (tension-and 3 3))
  (check-equal? (tension 3 4) (tension-and 3 4)))  

(define controls
  (case-lambda
    [(c)     (unless (pt? c) (error 'controls (~a "expected a pt, got: " c)))
             (controls-and c  c)]
    [(c- c+) (unless (and (pt? c-) (pt? c+)) 
               (error 'controls (~a "expected two pts, got: " c- " and " c+)))
             (controls-and c- c+)]))

(module+ test
  (defv (p q) (values (pt 1 0) (pt 2 3)))
  (check-equal? (controls p)   (controls p p))
  (check-equal? (controls p)   (controls-and p p))
  (check-equal? (controls p q) (controls-and p q)))

(define (tension-at-least τ)
  (unless (>= τ 3/4) (error 'tension-at-least "expected a positve tension of 3/4 or greater"))
  (tension-and (- τ) (- τ)))

(module+ test (check-equal? (tension-at-least 3) (tension-and -3 -3)))

(define (direction-specifier? x)
  (match x
    [(or (curl _) (vec _ _)) #t]
    [_ #f]))

(def ..  (tension-and 1 1))
(def ... (tension-at-least 1)) ; todo: negative tensions not supported yet
(def --- (tension 4095.99998)) ; "infinity" in MetaPost (largest representable number)
(def --  (full-join (curl 1) .. (curl 1)))


; A path has the form:  p0 j1 p1 j2 ... jn pn,
; where pi is a point or a path,  ji is a <path-join>.

; <path-join>           ::= <direction specifier> <basic path-join> <direction specifier>
;(struct join ()                    #:transparent)
;(struct tension-and  join (τ- τ+)  #:transparent) ; basic
;(struct controls-and join (c- c+)  #:transparent) ; basic
;(struct full-join join (ds- j ds+) #:transparent) ; full


(define cycle 'cycle)

(define (path* descr) (apply path: (flatten descr)))

#;(define (path: . descr)
  ; (displayln descr)
  (point/full-join-list->knots
   (replace-empty-direction-specifiers
    (override-direction-specifier-at-controls-and
     (introduce-explict-full-joins 
      descr)))))

(define (introduce-explict-full-joins descr)
  ; Rewrite descr to the form p0 fj0 p1 fj1 ... pn,
  ; where the fji are full-joins: (full-join ds- j ds+),
  ; where ds- and ds+ are direction specifiers 
  ; and j is a basic join: one of (curl γ), (vec x y) or #f.
  (defv (j? fj? ds?) (values join? full-join? direction-specifier?)) ; basic join
  (define (loop d)
    (match d
      [(list (? pt? pn)) d]
      [(list cycle)      d]
      [(list _) (error 'introduce-explict-full-joins (~a "expected pt or cycle, got: " d))]
      [(list* p            (? fj? fj)            d) (list* p fj (loop d))]
      [(list* p (? ds? ds-) (? j? j) (? ds? ds+) d) (list* p (full-join ds- j ds+) (loop d))]
      [(list* p (? ds? ds-) (? j? j)             d) (list* p (full-join ds- j #f)  (loop d))]
      [(list* p             (? j? j) (? ds? ds+) d) (list* p (full-join #f  j ds+) (loop d))]
      [(list* p             (? j? j)             d) (list* p (full-join #f  j #f)  (loop d))]
      [_ (error 'path: (~a "expected a path description, got: " d))]))
  (loop descr))

(module+ test
  (def iefj introduce-explict-full-joins)
  (let () (def fj full-join) (def γ (curl 2)) (def v (vec 3 4)) 
    (defv (p0 p1) (values (pt 1 2) (pt 3 4)))
    (check-equal? (iefj (list p0))                (list p0))
    (check-equal? (iefj (list p0 .. cycle))       (list p0 (fj #f .. #f) cycle))
    (check-equal? (iefj (list p0 .. p1))          (list p0 (fj #f .. #f) p1))
    (check-equal? (iefj (list p0 γ .. p1))        (list p0 (fj γ .. #f) p1))
    (check-equal? (iefj (list p0 .. γ p1))        (list p0 (fj #f .. γ) p1))
    (check-equal? (iefj (list p0 v .. γ p1))      (list p0 (fj v .. γ) p1))
    (check-equal? (iefj (list p0 (fj v .. γ) p1)) (list p0 (fj v .. γ) p1))))

(define (override-direction-specifier-at-controls-and dp)
  (def γ1 (curl 1))
  (define (replace p- fj? p+)
    (match fj?
      [(full-join d- (controls-and p q) d+)
       (full-join (if (pt~ p- p) γ1 (pt- p p-))
                  (controls-and p q)
                  (if (pt~ p+ q) γ1 (pt- p+ q)))]
      [_ fj?]))
  (def p0 (first dp))
  (define (loop dp)
    (match dp
      [(list p- fj 'cycle)
       (list p- (replace p- fj p0) cycle)]
      [(list p- fj p+ fj+ ...)
       (list* p- (replace p- fj p+) (loop (list* p+ fj+)))]
      [_ dp]))
  (loop dp))

(module+ test
  (define (oi dp) (override-direction-specifier-at-controls-and (introduce-explict-full-joins dp)))  
  (let () (def fj full-join) (defv (p0 p1 p2 p3) (values (pt 1 2) (pt 3 4) (pt 5 6) (pt 7 8)))
    (check-equal? (oi (list p0 (fj #f (controls-and p1 p2) #f) p3))
                  (list p0 (fj (pt- p1 p0) (controls-and p1 p2) (pt- p3 p2)) p3))))

(define (replace-empty-direction-specifiers dp)
  ; dp has the form p0 fj0 p1 fj1 ... pn   (pn might be cycle)
  ; Goal: Replace as many empty direction speciers as possible.
  ;       Only keep empty direction specifiers around an "open" point.
  ; Unless explicit overriding the incoming and outgoing tangent
  ; direction at a point p are the same.
  ; If only one of the directions are specified we need to duplicate it.
  ; Exception: If the join is controls-and (i.e. given explicit control points)
  ;            then no duplication must be done.
  ;            Empty direction specifiers next to constrols-and
  ;            are handled in replace-empty-direction-specifier-at-controls-and  
  ; For a middle point (i.e. non-start, non-end point p):  
  ;    replace  .. p d ..   with  .. d p d ..
  ;    replace  .. d p ..   with  .. d p d ..
  ; For closed curves all points are middle points.
  ; For an open curve the start (and end point) only has an outgoing (ingoing)
  ; direction, so no duplication is needed. If the directions specifier is
  ; empty, then the default direction specifier is (curl 1).
  (def γ1 (curl 1))
  (define (middle dp) ; dp = fj- p fj+ 
    (match dp
      [(list (full-join d1- j1 #f) p1 (full-join #f j2 d2+)) 
       ; open knot, nothing to duplicate
       (list (full-join d1- j1 #f) p1 (full-join #f j2 d2+))]
      [(list (full-join d1- j1 #f) p1 (full-join d2- j2 d2+))
       ; replace  .. p d ..   with  .. d p d ..
       (list (full-join d1- j1 d2-) p1 (full-join d2- j2 d2+))]
      [(list (full-join d1- j1 d1+) p1 (full-join #f j2 d2+))
       ; replace  .. d p ..   with  .. d p d ..
       (list (full-join d1- j1 d1+) p1 (full-join d1+ j2 d2+))]
      [(list (full-join d1- j1 d1+) p1 (full-join d2- j2 d2+))
       ; Keep the directions specifiers
       (list (full-join d1- j1 d1+) p1 (full-join d2- j2 d2+))]
      [_ (error 'middle (~a "internal error, dp=" dp))]))
  (define (middle-loop dp) ; fj0 p1 fj1 ... pn-1 fj-1 pn
    (match dp
      [(list* fj- p fj+ dp*)
       (defm (list fj-! p! fj+!) (middle (list fj- p fj+)))
       (list* fj-! p! (middle-loop (cons fj+! dp*)))]
      [(list fj-1)
       (list fj-1)]))
  (def closed? (member cycle dp))
  (cond
    [(not closed?) 
     (match dp
       [(list p0)       ;  n=0 => no middle (no joins)
        (list p0)]
       [(list p0 fj p1) ;  n=1 => no middle (one join)
        (defm (full-join d- j d+) fj)
        (list p0 (full-join (or d- γ1) j (or d+ γ1)) p1)]
       [(list p0 middle ... pn)
        (defm (list fj0 p1* ... fjn-) (middle-loop middle))
        (defm (full-join d0- j0 d0+)  fj0)
        (defm (full-join dn- jn- dn+) fjn-)
        (list* p0 (full-join (or d0- γ1) j0 d0+) ; empty ds after p0 => curl=1
               (append p1* 
                       (list (full-join dn- jn- (or dn+ γ1)) pn)))] ; ditto before pn
       )]
    [else ; closed (pn=cycle) => all nodes are middle nodes
     (defm (list p0 middle ... pn) dp)
     (def fjn- (last middle))
     (defm (list fj0- p0* ... fjn--) (middle-loop (list* fjn- p0 middle)))
     (defm (full-join _ j dn+) fj0-)
     (defm (full-join dn- _j_ _) fjn--)
     (append p0* (list (full-join dn- j dn+) cycle))]))

(module+ test
  (define (di dp) (replace-empty-direction-specifiers (introduce-explict-full-joins dp)))
  (def fj full-join) (def γ1 (curl 1)) (def v1 (vec 3 4)) (def v2 (vec 5 6))
  (def fj1 (fj #f .. #f))
  (let () (defv (p0 p1 p2 p3) (values (pt 1 2) (pt 3 4) (pt 5 6) (pt 7 8))) (def v (vec 1 2))
    (check-equal? (di (list p0 (fj #f .. #f) p1)) (list p0 (fj γ1 .. γ1) p1))
    (check-equal? (di (list p0 (fj #f .. v)  p1)) (list p0 (fj γ1 .. v) p1))   ; TODO correct?
    (check-equal? (di (list p0 (fj #f .. #f) p1 (fj v1 .. v2) p2 (fj #f .. #f) p3))
                  (list p0 (fj γ1 .. v1) p1 (fj v1 .. v2) p2 (fj v2 .. γ1) p3))
    (check-equal? (di (list p0 (fj #f .. #f) p1 (fj v1 .. #f) p2 (fj v2 .. #f) p3))
                  (list p0 (fj γ1 .. v1) p1 (fj v1 .. v2) p2 (fj v2 .. γ1) p3))
    (check-equal? (di (list p0 (fj #f .. v1) p1 (fj #f .. #f) p2 (fj v2 .. #f) p3))
                  (list p0 (fj γ1 .. v1) p1 (fj v1 .. v2) p2 (fj v2 .. γ1) p3))
    (check-equal? (di (list p0)) (list p0))
    (check-equal? (di (list p0 (fj #f .. #f) p1)) (list p0 (fj γ1 .. γ1) p1))
    (check-equal? (di (list p0 (fj #f .. v2) p1)) (list p0 (fj γ1 .. v2) p1))
    (check-equal? (di (list p0 (fj v1 .. #f) p1)) (list p0 (fj v1 .. γ1) p1))
    (check-equal? (di (list p0 .. cycle)) (list p0 (fj #f .. #f) cycle))
    (check-equal? (di (list p0 .. v1 cycle)) (list p0 (fj v1 .. v1) cycle))
    (check-equal? (di (list p0 v1 .. cycle)) (list p0 (fj v1 .. v1) cycle))
    (check-equal? (di (list p0 v1 .. p1 .. cycle)) (list p0 (fj v1 .. #f) p1 (fj #f .. v1) cycle))
    (check-equal? (di (list p0 v1 .. v2 p1 .. cycle)) (list p0 (fj v1 .. v2) p1 (fj v2 .. v1) cycle))
    (check-equal? (di (list p0 v1 .. p1 v2 .. cycle)) (list p0 (fj v1 .. v2) p1 (fj v2 .. v1) cycle))
    ))

(define (point/full-join-list->knots dp)
  ; dp = p0 fj0 p1 fj1 ... pn
  ; where fji is a full join ds- j ds+
  ; where j is a join: either tensions-and or controls-and
  ; Note: at this stage the join & has been eliminated.
  (def (sangle v) (reduce-angle/rad (angle v)))
  (define (categorize-left fj)
    (match fj
      [(full-join _ (tension-and _ τ) (curl γ))     (values (tenscurl τ γ) #f)]
      [(full-join _ (tension-and _ τ) (? vec? v))   (values (given τ (sangle v)) #f)]
      [(full-join _ (tension-and _ τ) #f)           (error 'categorize-left "internal error")] ; (*)
      [(full-join _ (controls-and _ c) (? vec? v))  (values (explicit) c)]
      [(full-join _ (controls-and _ c) _)           (error 'categorize-left "internal error")] ; (**)
      [_ (error 'categorize-left "internal error")]
      ; (*)  A single #f should not be possible - replace-empty-direction-specifiers has removed them
      ; (**) override-direction-specifier-at-controls-and ought to have introduced a vec here
      ))
  (define (categorize-right fj)
    (match fj
      [(full-join (curl γ)   (tension-and τ _) _ )  (values (tenscurl τ γ) #f)]
      [(full-join (? vec? v) (tension-and τ _) _)   (values (given τ (sangle v)) #f)]
      [(full-join #f         (tension-and τ _) _)   (error 'categorize-right "internal error")] ; (*)
      [(full-join (? vec? v) (controls-and c _) _)  (values (explicit) c)]
      [(full-join _          (controls-and c _) _)  (error 'categorize-right "internal error")] ; (**)
      [_ (error 'categorize-right "internal error")]
      ; (*)  A single #f should not be possible - replace-empty-direction-specifiers has removed them
      ; (**) override-direction-specifier-at-controls-and ought to have introduced a vec here
      ))
  (define (middle fj- p fj+)
    ; categorize the left and right type of each knot
    ;   i) left and right as open  (both ds are #f)
    ;  ii) tenscurl                (if both tension and curl are known)
    ; iii) given                   (angle - calcuated from vector)
    ;  iv) explicit                (control point has been explicitly given)
    ;   v) endpoint                (before start, and after end point in open curve)
    ; Note: The sixth type (endcycle) is not used here.
    ;       In later stages a it might be introduced to break an 
    ;       open curve without any break points.
    (defm (full-join _   j- ds-) fj-)
    (defm (full-join ds+ j+ _)   fj+)
    (match (list j- ds- ds+ j+)
      ; a knot is open only if both direction specifiers are #f
      ; (the joins must be tension-and, since direction specifiers,
      ;  adjacent to controls-and have been filled at this stage)
      [(list (tension-and _ τ-) #f #f (tension-and τ+ _))
       (knot p #f #f (open τ-) (open τ+) )]
      ; now we can categorize each side seperately
      [_ (defv (lt lc) (categorize-left fj-))
         (defv (rt rc) (categorize-right fj+))
         (knot p lc rc lt rt)]))
  (define (middle-loop dp) ; fj0 p1 fj1 ... pn-1 fj-1 pn
    (match dp
      [(list* fj- p fj+ dp*) (cons (middle fj- p fj+) (middle-loop (cons fj+ dp*)))]
      [(list fj-1)           '()]))
  (def closed? (member cycle dp))
  (cond
    [(not closed?)
     (open-path
      (match dp
        [(list p0)       ;  n=0 => no middle (no joins)
         (list (knot p0 p0 p0 (endpoint) (endpoint)))]
        [(list p0 fj p1) ;  n=1 => no middle (one join)
         (defv (rt rc) (categorize-right fj))
         (defv (lt lc) (categorize-left  fj))
         (list (knot p0 #f rc (endpoint) rt)
               (knot p1 lc #f lt (endpoint)))]
        [(list p0 middle ... pn)
         (def fj0 (first middle))
         (def fjn- (last middle))        
         (def ks (middle-loop middle))
         (defv (rt rc) (categorize-right fj0))
         (defv (lt lc) (categorize-left fjn-))
         (append (list (knot p0 #f rc (endpoint) rt))
                 ks
                 (list (knot pn lc #f lt (endpoint))))]))]
    [else ; closed (pn=cycle) => all nodes are middle nodes
     (defm (list p0 middle ... pn) dp)
     (def fjn- (last middle))
     (defm ks (middle-loop (list* fjn- p0 middle)))
     (def k0 (first ks))
     (closed-path (append ks (list k0)))]))

(define (path: . descr)  ; ; TODO handle controls-and
  ; (displayln (path-descr->point/join-list descr))
  (define  (fix-endpoints ks) ; copy information from k0.rt to kn.rt and from kn.lt to k0.lt.
    (defm (list k0 k ... kn) ks)
    (defm (knot p p- p+  lt rt) k0)
    (defm (knot q q- q+ qlt qrt) kn)
    (def lt* (if (and (open? qlt) (not (open? rt))) rt qlt)) ; rule R1 !    
    `(,(knot p p- p+ lt* rt) ,@k ,(knot q q- q+ lt* rt)))
  ; path-descr->point/join-list expands full joins... 
  (defv (pjs closed?) (path-descr->point/join-list descr))
  (def (sangle v) (reduce-angle/rad (angle v)))
  (def knots
    (let loop ([pjs pjs])
      (define (next k m) (cons k (loop m)))
      (match pjs
        ; first knot
        [(list* (? pt? p0) (and (full-join (curl γ) (tension-and τ- τ+) _) j1) m)
         (cons (knot p0 #f #f (endpoint) (tenscurl τ+ γ)) (loop (cons j1 m)))]
        [(list* (? pt? p0) (and (full-join (? vec? v) (tension-and τ- τ+) _) j1) m)
         (cons (knot p0 #f #f (endpoint) (given τ+ (sangle v))) (loop (cons j1 m)))]
        [(list* (? pt? p0) (and (full-join #f (tension-and τ- τ+) _) j1) m) ; beginning of cycle
         (cond [closed? (cons (knot p0 #f #f (endpoint) (open τ+)) (loop (cons j1 m)))] ; todo is this right
               [else    (error)])] ; never happens ?
        ; last knot
        [(list (full-join _ (tension-and τ- τ+) (curl γ)) pn)
         (list (knot pn #f #f (tenscurl τ+ γ) (endpoint)))]
        [(list (full-join _ (tension-and τ- τ+) (? vec? v)) pn)
         (list (knot pn #f #f (given τ+ (sangle v)) (endpoint)))]
        [(list (full-join _ (tension-and τ- τ+) #f) pn)
         (cond [closed? (list (knot pn #f #f (open τ-) (open τ+)))]
               [else    (list (knot pn #f #f (open τ-) (endpoint)))])]
        ; TODO handle controls-and
        ; middle knots
        [(list* (full-join _(tension-and _ τ-)(curl γ-)) p (and (full-join(curl γ+)(tension-and τ+ _)_ ) j) m)
         (next (knot p #f #f (tenscurl τ- γ-) (tenscurl τ+ γ+)) (cons j m))]
        [(list* (full-join _(tension-and _ τ-)(? vec? v)) p (and (full-join(curl γ+)(tension-and τ+ _)_ ) j) m)
         (next (knot p #f #f (given τ- (sangle v)) (tenscurl τ+ γ+)) (cons j m))]
        [(list* (full-join _(tension-and _ τ-)(curl γ-)) p (and (full-join(? vec? v)(tension-and τ+ _)_ ) j) m)
         (next (knot p #f #f (tenscurl τ- γ-) (given τ+ (sangle v)) ) (cons j m))]
        [(list* (full-join _(tension-and _ τ-)(? vec? v-)) p (and (full-join(? vec? v+)(tension-and τ+ _)_ ) j) m)
         (next (knot p #f #f (given τ- (sangle v-)) (given τ+ (sangle v+)) ) (cons j m))]
        [(list* (full-join _(tension-and _ τ-) #f)       p (and (full-join #f (tension-and τ+ _)_ ) j) m)
         (next (knot p #f #f (open τ-) (open τ+) ) (cons j m))]
        [(list* (full-join _(tension-and _ τ-) (? vec? v)) p (and (full-join #f (tension-and τ+ _)_ ) j) m) ; XXX!
         (next (knot p #f #f (given τ- (sangle v)) (given τ+ (sangle v))) (cons j m))]
        [(list* (full-join _(tension-and _ τ-) #f)       p (and (full-join (? vec? v) (tension-and τ+ _)_ ) j) m)
         (next (knot p #f #f (given τ- (sangle v)) (given τ+ (sangle v))) (cons j m))]
        [(list* (full-join _(tension-and _ τ-) (curl γ)) p (and (full-join #f (tension-and τ+ _)_ ) j) m)
         (next (knot p #f #f (tenscurl τ- γ) (tenscurl τ+ γ)) (cons j m))]
        [(list* (full-join _(tension-and _ τ-) #f)       p (and (full-join (curl γ) (tension-and τ+ _)_ ) j) m)
         (next (knot p #f #f (tenscurl τ- γ) (tenscurl τ+ γ)) (cons j m))]
        )))
  (cond [closed? (closed-path (fix-endpoints knots))]
        [else    (open-path knots)]))

(module+ test
  (defv (p0 p1 p2 p3 p4) (values (pt 0 0) (pt 1 1) (pt 2 0) (pt 3 0) (pt 4 1)))
  ; PHASE 3
  ; empty dirspec at start and end (of open curve) is replaces with (curl γ=1)
  (check-equal? (path: p0 .. p1)       (path: p0 (curl 1) .. (curl 1) p1))
  (check-equal? (path: p0 .. p1)       (path: p0 (curl 1) ..          p1))
  (check-equal? (path: p0 .. p1)       (path: p0          .. (curl 1) p1))
  (check-equal? (path: p0 .. p1 .. p2) (path: p0 (curl 1).. p1 .. (curl 1) p2))
  (check-equal? (path: p0 .. p1 .. p2) (path: p0 (curl 1).. p1 ..          p2))
  (check-equal? (path: p0 .. p1 .. p2) (path: p0         .. p1 .. (curl 1) p2))
  ; TODO: same tests for empty dirspec next to &
  #;(def c (curve p2 .. p3))
  #;(check-equal? (path: p0 .. p1 & c)       (path: p0 .. p1 (curl 1) & c))
  #;(check-equal? (path: p0 .. p1 & c .. p4) (path: p0 .. p1 (curl 1) & c .. p4))
  #;(check-equal? (path: c & p4)             (path: c (curl 1) .. p4))
  ; PHASE 4
  ; In the middle of a path: empty direction specifers are replaced with direction specifier
  ; in the other side of the point.
  (defv (d1 d2) (values (vec 1 2) (curl 3)))
  (for ([d (list (vec 1 2) (curl 3))])
    (check-equal? (path: p0 ..   p1 d .. p2)       (path: p0 .. d p1 d .. p2))
    (check-equal? (path: p0 .. d p1   .. p2)       (path: p0 .. d p1 d .. p2))
    (check-equal? (path: p0 ..   p1 d .. p2 .. p3) (path: p0 .. d p1 d .. p2 .. p3))
    (check-equal? (path: p0 .. d p1   .. p2 .. p3) (path: p0 .. d p1 d .. p2 .. p3))
    (check-equal? (path: p4 .. p0 ..   p1 d .. p2 .. p3) (path: p4 .. p0 .. d p1 d .. p2 .. p3))
    (check-equal? (path: p4 .. p0 .. d p1   .. p2 .. p3) (path: p4 .. p0 .. d p1 d .. p2 .. p3))
    (check-equal? (path: p4         .. d p0   .. d1 p1    ..    p2 d2 ..          p3) 
                  (path: p4 (curl 1).. d p0 d .. d1 p1 d1 .. d2 p2 d2 .. (curl 1) p3)))
  (for ([d (list (vec 1 2) (curl 3))])
    (check-equal? (path: p0 ..   p1 d .. p2 .. cycle)       (path: p0 .. d p1 d .. p2 .. cycle))
    (check-equal? (path: p0 .. d p1   .. p2 .. cycle)       (path: p0 .. d p1 d .. p2 .. cycle))
    (check-equal? (path: p0 ..   p1 d .. p2 .. p3 .. cycle) (path: p0 .. d p1 d .. p2 .. p3 .. cycle))
    (check-equal? (path: p0 .. d p1   .. p2 .. p3 .. cycle) (path: p0 .. d p1 d .. p2 .. p3 .. cycle))
    (check-equal? (path: p4 .. p0 ..   p1 d .. p2 .. p3 .. cycle) 
                  (path: p4 .. p0 .. d p1 d .. p2 .. p3 .. cycle))
    (check-equal? (path: p4 .. p0 .. d p1   .. p2 .. p3 .. cycle) 
                  (path: p4 .. p0 .. d p1 d .. p2 .. p3 .. cycle))
    (check-equal? (path: p4 .. d p0   .. d1 p1    ..    p2 d2 .. p3 .. cycle) 
                  (path: p4 .. d p0 d .. d1 p1 d1 .. d2 p2 d2 .. p3 .. cycle)))
  ; PHASE 5
  ; Empty direction specifier next to explicit control points
  (defv (u v) (values (pt 5 0) (pt 6 1)))
  #;(begin
      (check-equal? (path: p0 ..            p1 (controls u v) p4)    ; u<>p1
                    (path: p0 .. (pt- u p1) p1 (controls u v) p4))
      (check-equal? (path: p0 ..              p1 (controls p1 v) p4) ; u=p1
                    (path: p0 .. (curl 1)     p1 (controls p1 v) p4))
      (check-equal? (path: p0 (controls u v)  p1            .. p2)   ; v<>p1
                    (path: p0 (controls u v)  p1 (pt- p1 v) .. p2))
      (check-equal? (path: p0 (controls u p1) p1          .. p2)     ; v=p1
                    (path: p0 (controls u p1) p1 (curl 1) .. p2)))
  ; EQUAL CONSECUTIVE POINTS
  #;(check-equal? (path: p0 .. p0)             (path: p0 (controls-and p0 p0) p0))
  #;(check-equal? (path: p0 .. p0 .. cycle)    (path: p0 (controls-and p0 p0) p0 .. cycle))
  #;(check-equal? (path: p1 .. p0 .. p0 .. p2) (path: p1 .. p0 (controls-and p0 p0) p0 .. p2))
  #;(check-equal? (path: p1 .. p0 .. p0 .. p2 .. cycle) 
                  (path: p1 .. p0 (controls-and p0 p0) p0 .. p2 .. cycle))
  )


(define (path-descr->point/join-list descr)
  (def closed? (member 'cycle descr))
  (def last-dir #f) ; contains the dir-spec after last point, if present
  (define (expand-full-joins descr)
    ; -- is (full-join (curl 1) .. (curl 1)) 
    ; The removal of full joins make the remaining code simpler
    (def loop expand-full-joins)
    (match descr
      [(list)                          '()]
      [(list* (full-join ds- j ds+) m) `(,ds- ,j ,ds+ ,@(loop m))]
      [(list* x m)                     (cons x (loop m))]))
  (define (fix-last-dir)
    (match last-dir
      [#f pjs]
      [d  (defm (list pj ... (full-join d1 j d2) p) pjs)
          (append pj (list (full-join d1 j last-dir) p))])) ; todo: warn if d2 not #f ?  
  (def ds? direction-specifier?)
  (def fjoin? full-join?)
  (def c1 (curl 1))
  ; RULES (MetaFont Book, p. 130)
  ; R1 An empty dir-spec at the beginning or end of a path (or next to &) becomes {curl 1}.
  ;    In other words ..z{w} is treated as ..{w}z{w}
  ;    R1 does *not* apply to closed paths
  ; R2 A non-empty dir-spec after a point is copied into an empty one before.
  ; R3a when u<>z then .. z ..controls u and v..  is treated as  ..{u − z}z  .. controls u and v ..
  ; R3b when u =z then .. z ..controls u and v..  is treated as  ..{curl 1}z .. controls u and v ..
  ; R3c when v<>z then ..controls u and v.. z ..  is treated as  ..controls u and v.. z{z-v} ..
  ; R3d when v =z then ..controls u and v.. z ..  is treated as  ..controls u and v.. z{curl 1} ..
  (def expansion (expand-full-joins descr))
  (defm (list* p0 more)
    (match expansion
      [(list (pt x0 y0) (tension-and τ- τ+) (pt x1 y1) (tension-and τ1- τ1+) 'cycle)
       ; TODO: This special cases the two point cycle ...  This really ought to go...  !!!
       (defv (p0 p1) (values (pt x0 y0) (pt x1 y1)))       
       (list (pt x0 y0) (rot-90 (pt- p1 p0)) (tension-and 1 1) 
             (pt x1 y1) (rot90  (pt- p1 p0))   (tension-and 1 1) 'cycle)]
      [_ expansion]))
  
  (unless (pt? p0) 
    (error 'path: (~a "First element of a path must be a point (i.e. a pt structure). Got " p0)))
  (def pjs
    (match more
      ['() (list p0 -- p0)]  ; singleton path
      [_
       (cons 
        p0
        (let loop ([more (match more 
                           [(list* (? ds? d0) _)     more] 
                           [(? (λ(x) (not closed?))) (cons c1 more)] ; R1 
                           [_ more])]) 
          (define (next fj p m) (cons fj (cons p (loop m))))
          (match more
            ; last knot
            [(list             (? join? j)            (? pt? p)) (list (full-join #f j c1) p)] ; R1
            [(list  (? ds? d1) (? join? j)            (? pt? p)) (list (full-join d1 j c1) p)] ; R1
            [(list  (? ds? d1) (? join? j) (? ds? d2) (? pt? p)) (list (full-join d1 j d2) p)]
            [(list             (? join? j) (? ds? d2) (? pt? p)) (list (full-join #f j d2) p)] 
            ; last "knot" is cycle
            [(list             (? join? j)            'cycle) (list (full-join #f j #f) p0)] 
            [(list  (? ds? d1) (? join? j)            'cycle) (list (full-join d1 j #f) p0)]
            [(list  (? ds? d1) (? join? j) (? ds? d2) 'cycle) (list (full-join d1 j d2) p0)]
            [(list             (? join? j) (? ds? d2) 'cycle) (list (full-join #f j d2) p0)] 
            ; middle knots
            [(list* (? ds? d1) (? join? j)            (? pt? p) (? ds? d2) m) (next (full-join d1 j d2) p (cons d2 m))] ; ?         
            [(list*            (? join? j)            (? pt? p) (? ds? d2) m) (next (full-join #f j d2) p (cons d2 m))]
            ; - 
            [(list* (? ds? d1) (? join? j) (? ds? d2) (? pt? p) m) (next (full-join d1 j d2) p m)]
            [(list* (? ds? d1) (? join? j)            (? pt? p) m) (next (full-join d1 j #f) p m)] ; ?
            [(list*            (? join? j) (? ds? d2) (? pt? p) m) (next (full-join #f j d2) p m)] ; (not R2) !! xx
            [(list*            (? join? j)            (? pt? p) m) (next (full-join #f j #f) p m)]
            ; dir spec after last point
            [(list (? ds? d)) (set! last-dir d) '()] 
            [_ (error 'path: (~a "problem parsing the path " more))])))]))
  (values (fix-last-dir) closed?))

;;;
;;; Printing (276)
;;;

; Note: Printing paths in MetaPost syntax is used to test the results of this 
;       library against MetaPost. See draw-test.
; Todo: Test printing of closed paths.

(define (display-path p)
  (defm (path knots) p)
  (for ([(k- k k+) p])
    (match* (k- k k+)
      [(#f k k+) (display-adjacent-knots k k+)
                 (display-two-dots-possibly-followed-by-given-or-curl k+)]
      [(k- k #f) (display-pt (knot-p k))
                 (when (closed-path? p)
                   (display-two-dots-possibly-followed-by-given-or-curl k)
                   (display "cycle"))]
      [(k- k k+) (display-adjacent-knots k k+)
                 (display-two-dots-possibly-followed-by-given-or-curl k+)])))

(define (display-adjacent-knots k k+) ; 277
  (defm (knot p p- p+  lt  rt) k)
  (display-pt p)
  (match rt
    [(endpoint)     (void)]
    [(explicit)     (display-control-points-between-two-points k k+)]
    [(open τ)       (display-info-for-curve-that-begins-with-open k k+)]
    [(tenscurl τ a) (display-info-for-curve-that-begins-with-curl-or-given k k+)]
    [(given τ a)    (display-info-for-curve-that-begins-with-curl-or-given k k+)]
    [_ (display "???")]) ; can't happen
  (defm (knot q q- q+  qlt  qrt) k+)
  (cond [(or (explicit? qlt) (endpoint? qlt)) (display "..control?")] ; can't happen
        [(= 1 (right-tension k) (left-tension k+)) (void)] ; default tensions not printed
        [else (display-tension-between k k+)]))

(define (two-dots-possibly-followed-by-given-or-curl k)
  (~a " .."
      (match (knot-left-type k)
        [(given τ angle)     (~a "{"(~(cos angle))","(~(sin angle))"}")]
        [(tenscurl τ amount) (~a "{curl "(~ amount)"}")]
        [_                   ""])))

(define (display-two-dots-possibly-followed-by-given-or-curl k) ; 278
  (display (two-dots-possibly-followed-by-given-or-curl k)))

(define (display-tension-between k k+) ; 279
  (defm (knot p p- p+  lt  rt) k)
  (defm (knot q q- q+ qlt qrt) k+)
  (def rτ  (right-tension k))
  (def qlτ (left-tension k+))
  (display "..tension ")
  (when (negative? rτ) (display "atleast "))
  (display (~(abs rτ)))
  (unless (= rτ qlτ)
    (display " and ")
    (when (negative? qlτ) (display "atleast "))
    (display (~(abs qlτ)))))

(define (display-control-points-between-two-points k k+) ; 280
  (defm (knot p p- p+  lt  rt) k)
  (defm (knot q q- q+ qlt qrt) k+)
  (display "..controls ")
  (display-pt p+)
  (display " and ")
  (display-pt q-))

(define (display-info-for-curve-that-begins-with-open k k+) ; 281
  (defm (knot p p- p+  lt  rt) k)
  (defm (knot q q- q+ qlt qrt) k+)  
  (when (and (not (explicit? lt)) (not (open? lt)))
    (display "{open?}"))) ; can't happen

(define (display-info-for-curve-that-begins-with-curl-or-given k k+) ; 282
  (defm (knot p p- p+  lt  rt) k)
  (defm (knot q q- q+ qlt qrt) k+)
  (when (open? lt) (display "???")) ; can't happen
  (match rt
    [(tenscurl τ amount) (display (~a "{curl "(~ amount)"}"))]
    [(given τ angle)     (display (~a "{"(~(cos angle))","(~(sin angle))"}"))]))

(define (~pt p)
  (defm (pt px py) p)
  (~a "("(~ px)","(~ py)")"))

(define (display-pt p)  
  (display (~pt p)))

(define (~ x)
  ; todo: handle +inf.0  (relevant for ---)
  
  (~r (cond [(> x  4095.99998)  4095.99998]
            [(< x -4095.99998) -4095.99998]
            [else x])
      #:notation 'positional
      #:precision 5))

