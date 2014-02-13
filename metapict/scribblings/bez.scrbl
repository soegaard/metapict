#lang scribble/manual
@(require (for-label metapict (except-in racket angle box open path? identity ...))
          scribble/extract scribble/eval scribble/base scribble/manual "utils.rkt")
@(define eval (make-metapict-eval))
@interaction-eval[#:eval eval (require metapict)]
@(define math-style tt)

@title[#:tag "ref-bez"]{Bezier Curves}

@defmodule[metapict/bez]

@; ----------------------------------------

A Bezier curve from point @racket[A] to point @racket[B] with
control points @racket[A+] and @racket[B-] is represented as
an instance of a @racket[bez] structure: @racket[(bez A A+ B- B)].

Graphically such a curve begins at point @racket[A] and ends in point @racket[B].
The curve leaves point @racket[A] directed towards 
the control point @racket[A+]. The direction in which the curve enters the
end point @racket[B] is from @racket[B-]. 

The points @racket[A] and @racket[B] are referred to as start and end point 
of the Bezier curve. The points @racket[A+] and @racket[B-] are refererred
to as control points. The point @racket[A+] is the post control of @racket[A]
and the point @racket[B-] is the pre control of @racket[B].

Most users will not have reason to work with @racket[bez] structures directly.
The @racket[curve] constructor is intended to cover all use cases.

Each point on the Bezier curve corresponds to a real number @math{t} between 0 and 1.
The correspondence is called a @math{parameterization} of the curve. The number @math{t}
is called a @math{parameter}. Thus for each value of the parameter @math{t} between 0 and 1,
you get a point on the curve. The parameter value @math{t=0} corresponds to the start 
point @racket[A] and the parameter value @math{t=1} corresponds to the end point.

Let's see an example of a Bezier curve and its construction.
@begin[
(require metapict "construction-of-bezier-curve.rkt")                     
(let ()
  (def c (curve (pt 0 0) (vec -0.5 2) .. (vec -2 -2) (pt 5 0)))
  (set-curve-pict-size 120 120)
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

@defproc[(bez~ [b1 bez?] [b2 bez?] [ε real? 0.0001]) boolean?]{
Returns @racket[#t] if the defining points of the two Bezier curves 
are within pairwise distance @racket[ε] of each other.
The default value of @math{ε=0.0001} was chosen
to mimick the precision of MetaPost.}
@interaction[#:eval eval 
                    (bez~ (bez (pt 0     0) (pt 0 1) (pt 3 2) (pt 5 0))
                          (bez (pt 0.00005 0) (pt 0 1) (pt 3 2) (pt 5 0)))]

@defproc[(bez-reverse [b bez?]) bez]{
Returns a @racket[bez] representing a Bezier curve whose graph is the same as 
the graph of the Bezier curve @racket[b], but has the reverse orientation.}
@interaction[#:eval eval
  (def b     (bez (pt 0 0) (pt 0 1) (pt 3 2) (pt 5 0)))
  (def (γ t) (point-of-bez              b  t))
  (def (φ t) (point-of-bez (bez-reverse b) t))
  (def ts    (in-range 0 5/4 1/4))
  (cons 'γ (for/list ([t ts]) (γ t)))
  (cons 'φ (for/list ([t ts]) (φ t)))]

@defproc[(split-bez [b bez?] [t real?]) (values bez? bez?)]{
Given a Bezier curve @racket[b] from @math{p_0} to @math{p_3} with control
points @math{p_1} and @math{p_2}, split the Bezier curve at time @math{t} 
in two parts @math{b_1} (from @math{p_0} to @math{b(t)}) and @math{b_2} 
(from @math{b(t)} to @math{p_3}),
such that @racket[(point-of-bez b1 1)] = @racket[(point-of-bez b2 0)]
and the graphs of @math{b_1} and @math{b_2} gives the graph of @racket[b].}
@interaction[#:eval eval
  (def b        (bez (pt 0 0) (pt 0 1) (pt 3 2) (pt 5 0)))
  (defv (b1 b2) (split-bez b 1/3))
  (with-window (window -1 6 -1 6) 
    (penwidth 4
      (draw (color "red"  (draw b1))
            (color "blue" (draw b2)))))]

@defproc[(bez-subpath [b bez?] [t0 real?] [t1 real?]) bez?]{
Given a Bezier curve @racket[b] return a new Bezier curve @racket[c], 
such that @math{c(0)=b(t_0)} and @math{c(1)=b(t_1)} and
such that the graph of @racket[c] is a subset of the graph of @racket[b].}
@interaction[#:eval eval
  (def b (bez (pt 0 0) (pt 0 1) (pt 3 2) (pt 5 0)))
  (with-window (window -1 6 -1 6) 
    (for/draw ([t (in-range 0 5/4 1/4)]
               [c '("red" "blue" "green" "magenta")])              
    (penwidth 4 
      (beside (draw b) 
              (color c (draw (bez-subpath b t (+ t 1/4))))))))]
Note: The example shows that the parameterization is not an arc-length (aka unit-speed) 
parameterization.

@defproc[(bez-intersection-point [b1 bez?] [b2 bez?]) (or pt? #f)]{
If the graphs of the Bezier curves intersect, then their
first intersection point is returned. If there are no intersections,
then @racket[#f] is returned.}
@interaction[#:eval eval
  (def b1 (bez (pt 0. 0.) (pt 1. 1.) (pt 2. 2.) (pt 3. 3.)))
  (def b2 (bez (pt 0. 3.) (pt 1. 2.) (pt 2. 1.) (pt 3. 0.)))
  (defv (p) (bez-intersection-point b1 b2))
  p
  (def b3 (bez (pt 0 4) (pt 1 4) (pt 2 4) (pt 3 4)))
  (bez-intersection-point b1 b3)
  (with-window (window 0 5 0 5)
    (draw b1 b2 (color "red" (penwidth 8 (draw p))) b3))]

@defproc[(bez-intersection-times [b1 bez?] [b2 bez?]) (values real? real?)]{
If the graphs of the Bezier curves intersect numbers @math{t_1} and @math{t_2}
such that @math{b_1(t_1)=b_2(t_2)} are returned. If there are more than one
intersection, the parameter values for the first intersection is returned.
If no such numbers exist the result is @racket[(values #f #f)]. }
@interaction[#:eval eval
  (def b1 (bez (pt 0 0) (pt 1 1) (pt 2 2) (pt 3 3)))
  (def b2 (bez (pt 0 3) (pt 1 2) (pt 2 1) (pt 3 0)))
  (defv (t1 t2) (bez-intersection-times b1 b2))
  (defv (p1 p2) (values (point-of-bez b1 t1) (point-of-bez b2 t2)))
  (list p1 p2)
  (def b3 (bez (pt 0 4) (pt 1 4) (pt 2 4) (pt 3 4)))
  (bez-intersection-times b1 b3)
  (with-window (window 0 5 0 5)
    (draw b1 b2 (color "red" (penwidth 8 (draw p1))) b3))]

@defproc[(bez-intersection-point-and-times [b1 bez?] [b2 bez?]) 
         (or (list pt? real? real?) #f)]{
If the graphs of the Bezier curves intersect, returns a list of
the intersection point and two numbers @math{t_1} and @math{t_2} 
such that  @math{b_1(t_1)=b_2(t_2)}.
If there are more than one intersection, the parameter values for the first 
intersection is returned. If no such numbers exist the result is @racket[(values #f #f)]. }
@interaction[#:eval eval
  (def b1 (bez (pt 0. 0.) (pt 1. 1.) (pt 2. 2.) (pt 3. 3.)))
  (def b2 (bez (pt 0. 3.) (pt 1. 2.) (pt 2. 1.) (pt 3. 0.)))
  (bez-intersection-point-and-times b1 b2)
  (defm (list p t1 t2) (bez-intersection-point-and-times b1 b2))
  (def b3 (bez (pt 0 4) (pt 1 4) (pt 2 4) (pt 3 4)))
  (bez-intersection-times b1 b3)
  (with-window (window 0 5 0 5)
    (draw b1 b2 (color "red" (penwidth 8 (draw p))) b3))]

@defproc[(draw-bez [dc (is-a dc<%>)] [b bez?] 
                   [#:transformation t        trans? #f] 
                   [#:pen-transformation pent trans? #f]) 
         (void)]{
Draws the Bezier curve @racket[b] on a drawing context @racket[dc] with 
optional transformation @racket[t] and pen-transformation @racket[pent].}

@defproc[(draw-bezs [dc (is-a dc<%>)] [bs (listof bez?)]
                    [#:transformation t        trans? #f]
                    [#:pen-transformation pent trans? #f])
         (void)]{
Draws the Bezier curves @racket[bs] on the drawing context @racket[dc] with 
optional transformation @racket[t] and pen-transformation @racket[pent].}

@defproc[(bez->dc-path [b bez?] [t trans? #f]) (is-a? dc<%>)]{
Convert the Bezier curve @racket[b] into a @racket[dc-path%].
If the optional transformation @racket[t] is present, it is applied 
to @racket[b] before the conversion.}

@defproc[(bezs->dc-path [bs (listof bez?)] [t trans? #f]) (is-a? dc<%>)]{
Convert the "consecutive" Bezier curves @racket[bs] into a @racket[dc-path%].
If the optional transformation @racket[t] is present, it is applied 
to the @racket[bs] before the conversion.}

@defproc[(bez/dirs+tensions [p0 pt?]     [p3 pt?] 
                            [w0 vec?]    [w3 vec?] 
                            [τ0 real? 1] [τ3 real? 1]) bez?]{
Returns a @racket[bez] structure representing a Bezier curve from @racket[p0] to @racket[p3] 
that leaves @racket[p0] in the direction of @racket[w0]
and arrives in @racket[p3] from the the direction of @racket[w0] 
with tensions @racket[t0] and @racket[t3] respectively.}
@interaction[#:eval eval 
                    (defv (p0 p3 w0 w3 τ0 τ3) (values (pt 0 0) (pt 5 0) (vec 0 1) (vec 0 -1) 1 1))
                    (def b (bez/dirs+tensions p0 p3 w0 w3 τ0 τ3))
                    b
                    (with-window (window -5 11 -5 11) (draw b))]

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

