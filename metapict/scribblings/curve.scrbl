#lang scribble/manual
@(require (for-label metapict (except-in racket angle box open path? identity ...))
          scribble/extract scribble/eval scribble/base scribble/manual "utils.rkt")
@(define eval (make-metapict-eval))
@interaction-eval[#:eval eval (require metapict)]
@(define math-style tt)

@title[#:tag "ref-curve"]{Curves}

@defmodule[metapict/curve]

@; ----------------------------------------

General curves are drawn by gluing together a series of Bezier curves.
Conceptually a curve consists of multiple Bezier curves glued together.
Such a curve can be either open or closed (a loop).

The representation of a curve consists simply of a list of Bezier curves
and a flag indicating whether the curve is closed. 
For two consecutive Bezier curves in the list, the end point of the first
and the start point of the second must be equal.

The actual representation uses a @racket[curve:] structure:
@racket[(struct curve: (closed? bezs) #:transparent #:reflection-name 'curve)].

Most users will not have reason to work with @racket[curve:] structures directly.
The @racket[curve] constructor is intended to cover all use cases. The constructor
can be used to construct both curved as well as straight lines.

The syntax of @racket[curve] will be detailed later, but let's look at some examples.
First when mulitple points separated by @racket[..] are given, a smooth curve through
the points will be constructed: @racket[(curve p0 .. p1 .. p2 .. p3 .. p4)].

As a concrete example, let's look at the points
  @math{(0,0) (60,40) (40,90) (10,70) (30,50)}.

@interaction[#:eval eval
  (def p0 (pt 0 0))
  (def p1 (pt 60 40))
  (def p2 (pt 40 90))
  (def p3 (pt 10 70))
  (def p4 (pt 30 50))
  (define (label-points)
    (for/draw ([i '(0 1 2 3 4)]
               [p (list p0 p1 p2 p3 p4)]
               [d (list (lft) (rt) (top) (lft) (top))])
      (dot-label (~a i) p d)))
  (set-curve-pict-size 120 120)
  (with-window (window -20 100 -20 100)
    (draw (curve p0 .. p1 .. p2 .. p3 .. p4)
          (label-points)))]

In order to produce a closed curve, end the curve specification with @racket[.. cycle].
@interaction[#:eval eval
  (with-window (window -20 100 -20 100)
    (draw (curve p0 .. p1 .. p2 .. p3 .. p4 .. cycle)
          (label-points)))]

Use @racket[--] instead of @racket[..] if you want to connect two points with a
straight line.
@interaction[#:eval eval
  (with-window (window -20 100 -20 100)
    (draw (curve p0 .. p1 .. p2 -- p3 .. p4)
          (label-points)))]

There is more to the @racket[curve] constructor (it is possible to specify
directions in which a curve enters and leaves a points), but let's return
to operations defined on curves.

The number of Bezier curves used to represent the curve is called the length.
The function @racket[curve-length] returns this length @math{n}.
   
Each point on a curve corresponds to a real number @math{t} between 0 and @math{n}.
The correspondence is called a @math{parameterization} of the curve. The number @math{t}
is called a @math{parameter}. Thus for each value of the parameter @math{t} between 0 and @math{n},
you get a point on the curve. The parameter value @math{t=0} corresponds to the start 
point @racket[A] and the parameter value @math{t=n} corresponds to the end point.

@defproc[(curve-length [c curve?]) integer?]{
Returns the number of Bezier curves used to represent the curve @racket[c].
This number @math{n} is also the end of the interval @math{[0,n]} used to
parameterize the curve.
@interaction[#:eval eval
  (curve-length (curve p0 .. p1 .. p2 .. p3 .. p4))]}

Given a curve @racket[c] one can compute points on the curve corresponding to a parameter value
between 0 and the curve length with the function @racket[point-of].
@defproc[(point-of [c curve?] [t real?]) pt?]{
Given a curve @racket[c] and a number @racket[t] the function @racket[point-of]
computes the point corresponding to the parameter value @racket[t].
Here @racket[(point-of c 0)] and @racket[(point-of c (curve-length c))] will
return the start and and end point of the curve respectively.
@interaction[#:eval eval
  (let ()
    (define c (curve p0 .. p1 .. p2 .. p3 .. p4))
    (define (label-parameter-values)
      (for/draw ([i '(0 1 2 3 4)]
                 [d (list (lft) (rt) (top) (lft) (top))])
        (define p (point-of c i))
        (dot-label (~a "t=" i) p d)))
    (set-curve-pict-size 120 120)
    (with-window (window -20 100 -20 100)
      (draw c
            (label-parameter-values))))]}

Since the start and end point of a curve are used often, the following short hands are available:

@defproc[(start-point [c curve?]) pt?]{
Returns the start point of a curve.}

@defproc[(end-point [c curve?]) pt?]{
Returns the end point of a curve.}

@interaction[#:eval eval
  (let ()
    (def c (curve (pt 0 0) .. (pt 1 3) .. (pt 2 5)))
    (list (start-point c) (end-point c)))]

Given a curve @racket[c] parameterized from 0 to @math{n} from
a start point to an end point, one can use @racket[curve-reverse]
to create a curve where the parameterization is reversed.

@defproc[(curve-reverse [c curve?]) curve?]{
Returns a curve throught the same points as the curve @racket[c].
In the parameterization 0 corresponds to the end point of @racket[c]
and @racket[(curve-length c)] corresponds to start point og @racket[c].
@interaction[#:eval eval
  (let ()
    (def c (curve (pt 0 0) .. (pt 1 3) .. (pt 2 5)))
    (def r (curve-reverse c))
    (list (start-point c) (end-point c))
    (list (start-point r) (end-point r)))]}


Another way to produce a new curve is to join two existing curves.
@defproc[(curve-append [c1 curve?] [c2 curve?]) curve?]{
Given two curves @racket[c1] and @racket[c2] where the end point of @racket[c1]
is the start point of @racket[c2], the function @racket[curve-append] will
produce a curve that combines @racket[c1] and @racket[c2].
@interaction[#:eval eval
  (let ()
    (def c1 (curve (pt 0 0) .. (pt 1 1)))
    (def c2 (curve (pt 1 1) .. (pt 2 0)))
    (def c  (curve-append c1 c2))    
    (with-window (window 0 2 0 2)
       (draw (linewidth 6 (color "red"   (draw c1)))
             (linewidth 6 (color "blue"  (draw c2)))
             (linewidth 2 (color "white" (draw c))))))]
}

We will now turn to operations involving two curves. The first problem
we will look at is intersections between two curves.

@defproc[(intersection-times [c1 curve?] [c2 curve?]) (or #f number?)]{
If the two curves @racket[c1] and @racket[c2] have an intersection point,
the function will return times (parameter values) @math{t} and @math{u} such that
@math{c1(t)=c2(t)}. If no intersection point is found @racket[#f] is returned.
@interaction[#:eval eval
  (let ()
    (def c1 (curve (pt 0 0) .. (pt 2 3)))
    (def c2 (curve (pt 0 2) .. (pt 4 1)))
    (defv (t u) (intersection-times c1 c2))
    (list (list 't t 'u u)
          (list (point-of c1 t) (point-of c2 u))))]
}

@defproc[(intersection-point [c1 curve?] [c2 curve?]) (or #f pt?)]{
Return an intersection point of the curves @racket[c1] and @racket[c2].
If no intersection point exists, the false value @racket[#f] is returned.
@interaction[#:eval eval
  (let ()
    (def c1 (curve (pt 0 0) .. (pt 2 4)))
    (def c2 (curve (pt 0 4) .. (pt 4 0)))
    (def P  (intersection-point c1 c2))
    (with-window (window 0 4 0 4)
      (draw c1 c2 P (dot-label-rt "P" P))))]}

If you need both the intersection point and the two times, then fear not:
@defproc[(intersection-point-and-times [c1 curve?] [c2 curve?]) (or #f (list pt? number? number?))]{
Return a list @racket[(list P t u)] where @racket[P] is an intersection point of
the two curves @racket[c1] and @racket[c2] -- and the numbers @racket[t] and @racket[u]
are times such that @math{P=c1(t)=c2(t)}.}

Finally to get all intersection points of two curves, use @racket[intersection-points].
@defproc[(intersection-points [c1 curve?] [c2 curve?]) (list-of pt?)]{
Returns a list of all intersection points of the two curves @racket[c1] and @racket[c2].
If no intersection point exists, the empty list is returned.
@interaction[#:eval eval
  (let ()
    (def c1 (curve (pt 0 0) .. (pt 3 1) .. (pt 2 3) .. (pt 2 1) .. (pt 2.5 1)))
    (def c2 (curve (pt 0 1) .. (pt 1 3) .. (pt 3 0) .. (pt 4 1)))
    (def Ps  (intersection-points c1 c2))
    (with-window (window -1 4 -1 4)
      (draw c1
            (linewidth 2 (color "blue" (draw c2)))
            (color "red" (for/draw ([P Ps])
                           (dot-label "" P (cnt)))))))]
}

If a curve is too long, the function @racket[subcurve] can be used to
make a shorter one.

@defproc[(subcurve [c curve?] [t0 number?] [t1 number?]) curve?]{
Produces a curve from @math{c(t_0)} to @math{c(t_1)}. If @math{t_0<t_1}
the direction is reversed.
@interaction[#:eval eval
  (let ()
    (def c (curve (pt 0 0) up .. (pt 1 3) .. (pt 2 0) .. (pt 3 3) .. (pt 3 2)))
    (def s (subcurve c 1 3))
    (with-window (window -1 4 -1 4)
      (draw (linewidth 6 (color "red"   (draw c)))
            (linewidth 2 (color "white" (draw s))))))]}

Instead of @racket[subcurve] one can use @racket[cut-before] and @racket[cut-after]
to make curves shorter.
@defproc[(cut-before [c1 curve?] [c2 curve?]) curve?]{
Cut the part of @racket[c1] that lie before the "first" intersection point of
the two curves.
@interaction[#:eval eval
  (let ()
    (def c1 (curve (pt 0 0) .. (pt 2 2)))
    (def c2 (curve (pt 0 2) .. (pt 2 0)))
    (def c  (cut-before c1 c2))
    (with-window (window 0 2 0 2)
      (draw (linewidth 6 (color "red"   (draw c1)))
            (linewidth 6 (color "blue"  (draw c2)))
            (linewidth 2 (color "white" (draw c))))))]}


@defproc[(cut-after [c1 curve?] [c2 curve?]) curve?]{
Cut the part of @racket[c1] that lie after the "first" intersection point of
the two curves.
@interaction[#:eval eval
  (let ()
    (def c1 (curve (pt 0 0) .. (pt 2 2)))
    (def c2 (curve (pt 0 2) .. (pt 2 0)))
    (def c  (cut-after c1 c2))
    (with-window (window 0 2 0 2)
      (draw (linewidth 6 (color "red"   (draw c1)))
            (linewidth 6 (color "blue"  (draw c2)))
            (linewidth 2 (color "white" (draw c))))))]}

@defproc[(post-control [c curve?] [t number?]) pt?]{
The curve @racket[c] consists of a number of Bezier curves.
Return the first control point after the paramter value @racket[t].
See the MetaFontBook page 134.}

@defproc[(pre-control [c curve?] [t number?]) pt?]{
The curve @racket[c] consists of a number of Bezier curves.
Return the first control point before the paramter value @racket[t].
See the MetaFontBook page 134.}

@defproc[(direction-of [c curve?] [t number?]) vec?]{
Return the direction of which the curve @racket[c] is moving at time @racket[t].
@interaction[#:eval eval
  (let ()
    (def c (curve (pt 1 0) .. (pt 1 3) .. (pt 4 1)))
    (def p (point-of     c 1))
    (def d (direction-of c 1))
    (with-window (window 0 6 0 6)
      (draw c
            (draw-arrow (curve p .. (pt+ p d)))
            (dot-label-top "P" p))))]}

@defproc[(cyclic? [c curve?]) boolean?]{
Return the true value @racket[#t] if the curve @racket[c] is a closed curve.
@interaction[#:eval eval
  (let ()
    (def c1 (curve (pt 1 0) .. (pt 1 3)))
    (def c2 (curve (pt 1 0) .. (pt 1 3) .. cycle))
    (list (cyclic? c1) (cyclic? c2)))]}


@defproc[(intercurve [α number?] [c1 curve?] [c2 curve?]) curve?]{
Given two curves @racket[c1] and @racket[c2] of the same length,
the call @racket[(intercurve α c1 c2)] will return a curve "between"
the two curves. For @racket[α=0] the first curve is return and
for @racket[α=1] the second curve is returned.

In other words we can interpolate curves.

@interaction[#:eval eval
  (let ()
    (def heart @code:comment{a heart shaped curve}
      (curve (pt 100 162) .. (pt 140 178) right .. (pt 195 125) down 
             .. (pt 100 0) (curl 0) .. up (pt 5 125) .. right (pt 60 178) .. (pt 100 162)))

    (def fig-heart @code:comment{a heart pict}
      (with-window (window -100 300 -100 300) (draw heart)))

    (def heart-line @code:comment{used to interpolate from line to heart below}
      (curve (pt 100 0) -- (pt 300 0) -- (pt 200 0) -- (pt 100 0) -- (pt 0 0)
             -- (pt -100 0) -- (pt 100 0)))

    (def fig-line-to-heart @code:comment{shows interpolation between two curves of same length}
      (with-window (window -100 300 -100 300) 
        (draw* (for/list ([i (in-range 8)])
                 (def α (/ i 8))
                 (color α "red"
                        (draw (intercurve α heart-line heart)))))))

    (beside fig-heart fig-line-to-heart))]}
    
