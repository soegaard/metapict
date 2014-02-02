#lang scribble/manual
@(require (for-label (except-in racket angle open path? identity ...) metapict metapict/metapict/pt-vec)
          scribble/extract scribble/eval scribble/base scribble/manual "utils.rkt")
@(define eval (make-metapict-eval))
@interaction-eval[#:eval eval (require metapict)]
@(define math-style tt)

@title[#:tag "points-and-vectors"]{Points and Vectors (pt and vec)}
@(author-jens-axel)

@defmodule[metapict/pt-vec]

@local-table-of-contents[]

@; ----------------------------------------
@section[#:tag "points-and-vectors"]{Points and Vectors}

Points and vectors are represented as @racket[pt] and @racket[vec] structures respectively.
Think of points as positions and of vectors as displacements.

@defstruct[pt ([x real?] [y real?])]{
The @racket[pt] structure represents a point with coordinates
@math{(x,y)} in the current coordinate system.}

@interaction-eval[#:eval eval (set-curve-pict-size 50 50)]
@interaction[#:eval eval
  (def A (pt 3 4))
  A
  (pt-x A)
  (penwidth 4 (draw (pt 0 0) (pt 1/2 1/2) (pt 1 0)))]

@defstruct[vec ([x real?] [y real?])]{
The @racket[vec] structure represents a vector with coordinates
@math{(x,y)} in the current coordinate system.}
@interaction[#:eval eval
  (def v (vec 3 4))
  v
  (vec-x v)]

@subsection[]{Predefined Points and Vectors}
The most common points and vectors have predefined names.

@defthing[origo (pt 0 0)]{Origo @math{(0,0)} is the reference point of the coordinate system.}

@deftogether[( @defthing[north (vec  0  1)]
               @defthing[south (vec  0 -1)]
               @defthing[west  (vec -1  0)]
               @defthing[east  (vec  1  0)])]{
The compass directions as @racket[vec]s.
}

@deftogether[( @defthing[up    (vec  0  1)]
               @defthing[down  (vec  0 -1)]
               @defthing[left  (vec -1  0)]
               @defthing[right (vec  1  0)])]{
Alternative directions names. Note that the direction names makes sense only if, the 
current coordinate system has a positive orientation.
}

@interaction[#:eval eval
        (penwidth 4 (draw (color "red"    (draw      origo))
                          (color "green"  (draw (pt+ origo north)))
                          (color "blue"   (draw (pt+ origo south)))
                          (color "yellow" (draw (pt+ origo left)))
                          (color "purple" (draw (pt+ origo right)))))]

@subsection[]{Point Operations}

@deftogether[( @defproc[(pt+ [A pt?] [v vec?]) pt?]
               @defproc[(pt+ [A pt?] [B pt?])  pt?]
               @defproc[(pt+)                  pt?]
               @defproc[(pt+ [A pt?])          pt?]               
               @defproc[(pt+ [A pt] ...)       pt?])]{
The form @racket[(pt+ A v)] returns the displacement of the point 
@racket[A] with the vector @racket[v].

The form @racket[(pt+ A B)] adds the coordinates of @racket[A] and @racket[B] pairwise.
The point @racket[A] is thus displaced with the vector @racket[OB].

The form @racket[(pt+)] returns the origo @racket[(pt 0 0)].

The form @racket[(pt+ A)] returns the point @racket[A].

The form @racket[(pt+ A1 A2 ...)] returns the result of @racket[(pt+ A1 (pt+ A2 ...))]. }
@interaction[#:eval eval 
                    (pt+ (pt 1 2) (vec 3 7))
                    (pt+ (pt 1 2) (pt 3 7))
                    (pt+)
                    (pt+ (pt 1 2))
                    (pt+ (pt 0.3 0.4) (vec 3 0) (vec 4 0))]

@deftogether[(@defproc[(pt- [A pt?] [v vec?]) pt?]
              @defproc[(pt- [A pt?])          pt?])]{
The form @racket[(pt- A v)] returns the displacement of the point @racket[A] with 
the opposite of vector @racket[v].
                                                   
The form @racket[(pt- A)] returns the reflection of the point @racket[A] with respect to origo.}
@interaction[#:eval eval 
                    (pt- (pt 1 2) (vec 3 7))
                    (pt- (pt 1 2))]

@defproc[(pt* [s real?] [A pt?]) pt?]{
Scale the coordinates of @racket[A] with @racket[s].
If the coordinates of @racket[A] are @math{(x,y)} then the point @math{(sx,sy)} is returned.}
@interaction[#:eval eval (pt* 3 (pt 1 2))]

@defproc[(dist [A pt?] [B pt?]) real?]{
Return the distance between the points @racket[A] and @racket[B].

The distance from @math{(x,y)} to @math{(a,b)} is sqrt(@math{(x-a)^2 + (y-b)^2}).}
@interaction[#:eval eval (dist (pt 4 0) (pt 4 3))]

@defproc[(pt= [A pt?] [B pt?]) boolean?]{
Returns @racket[#t] if the coordinates of the point @racket[A] and @racket[B] are 
equal with respect to @racket[=]. Otherwise @racket[#f] is returned.}
@interaction[#:eval eval 
                    (pt= (pt 1 2) (pt 1 2))
                    (pt= (pt 1 2) (pt 1 42))]

@defproc[(pt~ [A pt?] [B pt?] [ε 1e-15]) boolean?]{
Returns @racket[#t] if the distance from the point @racket[A] to the point is 
less than or equal to @racket[ε]. The default value of @racket[ε] is 1e-15.}
@interaction[#:eval eval 
                    (pt~ (pt 1 2) (pt 1 2.09))
                    (pt~ (pt 1 2) (pt 1 2.09) 0.1)]

@defproc[(med [r real?] [A pt?] [B pt?]) pt?]{
The mediate function @racket[med] computes points between points @racket[A] 
and @racket[B] (if @math{0<=r<=1}). The mediation operation is also known
as @emph{linear interpolation}.

The form @racket[(med 1/3 A B)] returns the point that lies
one-third of the way from @racket[A] to @racket[B]. 

In general @racket[(med r A B)] returns the point @math{(1-r)A + rB}.}
@interaction[#:eval eval
                    (def A (pt 0 0)) (def B (pt 2 1))
                    (list (med 0 A B) (med 1/3 A B) (med 1/2 A B) (med 2/3 A B) (med 1 A B))
                    (set-curve-pict-size 100 50)
                    (with-window (window -0.2 2.2 -0.1 1.1)
                      (penwidth 4 (draw* (for/list ([r '(0 1/3 1/2 2/3 1)]
                                                    [c '("red" "orange" "yellow" "green" "blue")])
                                           (color c (draw (med r A B)))))))]

@defproc[(pt@ [r real?] [θ real?]) pt?]{
Returns the point with polar coordinations @math{(r,θ)}.
That is, the point is on the circle with center @math{(0,0)} and radius @math{r}.
The angle from the @math{x}-axis to the line through origo and the point is @math{θ}.
The angle @math{θ} is given in radians (0 rad = 0 degrees, π rad = 180 degrees).}
@interaction[#:eval eval
             (require racket/math) ; for pi
             (set-curve-pict-size 50 50)
             (with-window (window -1.1 1.1 -1.1 1.1)
               (penwidth 4 (draw* (for/list ([θ (in-range 0 (* 2 pi) (/ (* 2 pi) 12))])
                                    (pt@ 1 θ)))))]
             
@defproc[(pt@d [r real?] [θ real?]) pt?]{
Same as @racket[pt@] but the angle is in degrees.}
@interaction[#:eval eval 
                    (pt@d 1 45)
                    (pt@  1 (/ pi 4))]