#lang scribble/manual
@(require (for-label metapict
                     (except-in racket angle box open path? identity ...))
          scribble/extract scribble/eval scribble/base scribble/manual "utils.rkt")
@(define eval (make-metapict-eval))
@interaction-eval[#:eval eval (require metapict)]
@(define math-style tt)

@title[#:tag "pt-and-vec"]{Points and Vectors (pt and vec)}

@defmodule[metapict/pt-vec]

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
        (penwidth 4 (draw (color "red"     (draw      origo))
                          (color "green"   (draw (pt+ origo north)))
                          (color "blue"    (draw (pt+ origo south)))
                          (color "magenta" (draw (pt+ origo left)))
                          (color "purple"  (draw (pt+ origo right)))))]

@subsection[]{Point Operations}


@defproc*[( [(pt+ [A pt?] [v vec?]) pt?]
            [(pt+ [A pt?] [B pt?])  pt?]
            [(pt+ [A pt]  [B-or-v (or pt? vec?)] ...) pt?])]{
Let the coordinates of @math{A}, @math{B} and @math{v} be                                                              
@math{A=@coords{a}}, @math{B=@coords{b}}, and, @math{v=@coords{v}}.
                       
The form @racket[(pt+ A v)] returns the displacement of the point 
@racket[A] with the vector @racket[v]. 
That is, @math{(a_1+v_1,a_2+v_2)} is returned.

The form @racket[(pt+ A B)] adds the coordinates of @racket[A] and @racket[B] pairwise.
The point @racket[A] is thus displaced with the vector @racket[OB].
That is, @math{(a_1+b_1,a_2+b_2)} is returned.

The form @racket[(pt+)] returns origo, @racket[(pt 0 0)].

The form @racket[(pt+ A)] returns the point @racket[A].

The form @racket[(pt+ A B-or-v ...)] returns the result of @racket[(pt+ (pt+ A B-or-v) ...)]. }
@interaction[#:eval eval 
                    (pt+ (pt 1 2) (vec 3 7))
                    (pt+ (pt 1 2) (pt 3 7))
                    (pt+)
                    (pt+ (pt 1 2))
                    (pt+ (pt 0.3 0.4) (vec 3 0) (vec 4 0))]

@defproc*[( [(pt- [A pt?] [B pt?]) pt?]
            [(pt- [A pt?] [v vec?]) pt?]
            [(pt- [A pt?])          pt?])]{                                                     
The form @racket[(pt- B A)] returns the vector @math{AB}. That is, if @math{A=@coords{a}} and 
@math{B=@coords{b}}, then @math{(b_1-a_1,b_2-a_2)} is returned.
         
The form @racket[(pt- A v)] returns the displacement of the point @racket[A] with 
the opposite of vector @racket[v]. If @math{A=(a1,a2)} and @math{v=(v1,v2)} then
the vector @math{(a1-v1,a2-v2)} is returned.
                                                   
The form @racket[(pt- A)] returns the reflection of the point @racket[A] with respect to origo.
If @math{A=(a1,a2)}, then the vector @math{(-a1,-a2)} is returned.}
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

@subsection[]{Vector Operations}

In this section the coordinates of @racket[vec]s @math{v} and @math{w} 
will be referred to as @math{v=@coords{v}} and @math{w=@coords{w}}.

@defproc[(vec+ [v vec?] [w vec?]) vec?]{
Returns the vector sum of @math{v} and @math{w}, that is
the vector @math{(v_1+w_1,v_2+w_2)} is returned.

In terms of displacements the vector sum @math{v+w} can be
thought of as the result of the displament @math{v} followed by
the displacement @math{w}.}

@interaction[#:eval eval 
                    (def v   (vec 2 0))
                    (def w   (vec 0 3))
                    (def v+w (vec+ v w))
                    v+w
                    (define (arrow v [offset (vec 0 0)])
                      (def A (pt+ origo offset))
                      (draw-arrow (curve A -- (pt+ A v))))   ; adds arrow at the end of the curve
                    (ahlength (px 12))                       ; length of arrow head
                    (with-window (window -0.2 3.2 -0.2 3.2)
                      (penwidth 2
                        (draw (color "red"   (arrow v))
                              (color "green" (arrow w v))
                              (color "blue"  (arrow v+w)))))]

@defproc[(vec- [v vec?] [w vec?]) vec?]{
Returns the vector difference of @math{v} and @math{w}, that is
the vector @math{(v_1-w_1,v_2-w_2)} is returned.}

@defproc[(vec* [s real?] [v vec?]) vec?]{
Scale the coordinates of @racket[v] with @racket[s].
If the coordinates of @racket[v] are @math{(x,y)} then the vector @math{(sx,sy)} is returned.}
@interaction[#:eval eval (vec* 3 (vec 1 2))]

@defproc[(vec->pt [v vec?]) pt?]{
Converts the vector @math{(x,y)} into a point with the same coordinates.
If a point @math{A} has the same coordinates as a vector @math{v}, then
the vector is said to a position vector for the point and @math{OA=v}.}
@interaction[#:eval eval (vec->pt (vec 1 2))]

@defproc[(pos [p pt?]) vec?]{
Converts the point @racket[p] into a vector with the same coordinates.
Such a vector is also called a position vector, hence the name.}
@interaction[#:eval eval  (pos (pt 1 2))]

@defproc[(vec= [v vec?] [w vec?]) boolean?]{
Returns @racket[#t] if the coordinates of the vectors @racket[v] and @racket[w] are 
equal with respect to @racket[=]. Otherwise @racket[#f] is returned.}
@interaction[#:eval eval 
                    (vec= (vec 1 2) (vec 1 2))
                    (vec= (vec 1 2) (vec 1 42))]

@defproc[(vec~ [v vec?] [w vec?] [ε 1e-15]) boolean?]{
Returns @racket[#t] if the length of @math{v-w} is less than or equal to @racket[ε]. 
The default value of @racket[ε] is 1e-15.}
@interaction[#:eval eval 
                    (vec~ (vec 1 2) (vec 1 2.09))
                    (vec~ (vec 1 2) (vec 1 2.09) 0.1)]

@defproc[(dot [v vec?] [w vec?]) real?]{
Returns the dot product of the vectors @racket[v] and @racket[w].
The dot product is the number @nonbreaking{@math{v_1 w_1 + v_2 w_2}}.

The dot product of two vectors are the same as the product of the lengths 
of the two vectors times the cosine of the angle between the vectors.
Thus the dot product of two orthogonal vectors are zero, and the
dot product of two vectors sharing directions are the product of their lengths.}
@interaction[#:eval eval 
                    (dot (vec 1 0) (vec 0 1))
                    (dot (vec 0 2) (vec 0 3))]

@defproc[(len2 [v vec?]) real?]{
Returns the square of the length of the vector @racket[v].}
@interaction[#:eval eval 
                    (len2 (vec 1 1))
                    (len2 (vec 3 4))]

@defproc*[([(len  [v vec?]) real?]
           [(norm [v vec?]) real?])]{
Returns the length of the vector @racket[v].}
@interaction[#:eval eval 
                    (len (vec 1 1))
                    (len (vec 3 4))]

@defproc[(dir/rad [α real?]) vec?]{
Returns the unit vector whose angle with the first axis is @math{α} radians.}
@interaction[#:eval eval 
                    (dir/rad 0)
                    (dir/rad (/ pi 2))]

@defproc[(dir [α real?]) vec?]{
Returns the unit vector whose angle with the first axis is @math{α} degrees.}
@interaction[#:eval eval 
                    (dir 0)
                    (dir 90)]

@defproc[(vec@ [r real?] [α real?]) vec?]{
Returns the vector of length @racket[r] whose angle with the first axis is @math{α} radians.
In other words construct a vector form polar coordinates.}
@interaction[#:eval eval 
                    (vec@ 2 0)
                    (vec@ 2 pi)]

@defproc[(|@| [A-or-v (or pt? vec?)]) (values real? real?)]{
Returns the polar coordinates of the point or vector.}
@interaction[#:eval eval 
                    (|@| (pt  3 4))
                    (|@| (vec 3 4))]

@defproc*[([(rot90  [A-or-v (or pt? vec?)]) (or pt? vec?)]
           [(rot-90 [A-or-v (or pt? vec?)]) (or pt? vec?)])]{
Rotates the point or vector 90 or -90 degrees around origo.}
@interaction[#:eval eval 
                    (rot90  (pt  1 0))
                    (rot90  (vec 1 0))
                    (rot-90 (pt  1 0))
                    (rot-90 (vec 1 0))]

















