#lang scribble/manual
@(require (for-label metapict (except-in racket angle box open path? identity ...))
          scribble/extract scribble/eval scribble/base scribble/manual "utils.rkt")
@(define eval (make-metapict-eval))
@interaction-eval[#:eval eval (require metapict)]
@(define math-style tt)

@title[#:tag "bez"]{Bezier Curves}

@defmodule[metapict/bez]

@local-table-of-contents[]

@; ----------------------------------------
@section[#:tag "ref-bez"]{Bezier Curves}

A Bezier curve from point @racket[A] to point @racket[B] with
control points @racket[A+] and @racket[B-] is represented as
an instance of a @racket[bez] structure: @racket[(bez A A+ B- B)].

Graphically such a curve begins at point @racket[A] and ends in point @racket[B].
The direction in which the curve leaves point @racket[A] is directed towards 
the control point @racket[A+]. The direction in which the curve enters the
end point @racket[B] is direction from @racket[B-]. 

The points @racket[A] and @racket[B] are referred to as start and end point 
of the Bezier curve. The points @racket[A+] and @racket[B-] are refererred
to as control points. The point @racket[A+] is the post control of @racket[A]
and the point @racket[B-] is the pre control of @racket[B].

Most users will not have reason to work with @racket[bez] structures directly.
The @racket[curve] constructor is intended to cover all use cases.

Each point on the Bezier curve is corresponds to a real number @math{t} between 0 and 1.
The correspondance is called a @math{parametrization} of the curve. The number @math{t}
is called a @math{parameter}. Thus for each value of the parameter @math{t} between 0 and 1,
you get a point on the curve. The parameter value @math{t=0} corresponds to the start 
point @racket[A] and the parameter value @math{t=1} corresponds to the end point.

Let's see an example of a Bezier curve and its construction.}
@interaction-eval[(require "construction-of-bezier-curve.rkt")]
@begin[
(require metapict "construction-of-bezier-curve.rkt")                     
(let ()
  (def c (curve (pt 0 0) (vec -0.5 2) .. (vec -2 -2) (pt 5 0)))
  (set-curve-pict-size 180 180)
  (define (from-to from to)
    (with-window (window -5 11 -5 11)
      (for/list ([t (in-range from to 1/11)])
        (draw-bezier-diagram c t "A" "A+" "B-" "B"))))
  (above (apply beside (from-to 0     4/11))
         (apply beside (from-to 4/11  8/11))
         (apply beside (from-to 8/11 12/11))))]

@defproc[(point-of-bez [b bez?] [t real?]) pt?]{
Return the point on the Bezier curve @racket[b] that corresponds to the parameter value @racket[t].
De Casteljau's algorithm is used to compute the point.}
@interaction[#:eval eval
  (def b (bez (pt 0 0) (pt 0 1) (pt 3 2) (pt 5 0)))
  (for/list ([t '(0 1/2 1)])
    (point-of-bez b t))]

@defproc[(bez~ [b1 bez?] [b2 bez?] [ε 0.0001]) boolean?]{
Returns @racket[#t] if the defining points of the two Bezier curves
are within a distance of ε. The default value of ε=0.0001 was chosen
to mimick the precision of MetaPost.}
@interaction[#:eval eval 
                    (bez~ (bez (pt 0       0) (pt 0 1) (pt 3 2) (pt 5 0))
                          (bez (pt 0.00005 0) (pt 0 1) (pt 3 2) (pt 5 0)))]

@defproc[(control-points [p0 pt?]   [p3 pt] 
                         [θ  real?] [φ  real?] 
                         [τ0 real?] [τ3 real?]) bez?]{
Returns a @racket[bez] structure representing a Bezier curve from @racket[p0] to @racket[p3] 
that leaves @racket[p0] with an angle of @racket[θ]
and arrives in @racket[p3] with an angle of @racket[φ] 
with tensions @racket[t0] and @racket[t3] respectively.}
@interaction[#:eval eval 
                    (defv (p0 p3 θ φ τ0 τ3) (values (pt 0 0) (pt 5 0) pi/2 -pi/2 1 1))
                    (defv (p1 p2) (control-points p0 p3 θ φ τ0 τ3))
                    (def b (bez p0 p1 p2 p3))
                    b
                    (with-window (window -5 11 -5 11) (draw b))]







