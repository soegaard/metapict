#lang scribble/manual
@(require (for-label metapict racket/draw
                     (except-in racket angle box open path? unit identity ...))
          scribble/extract scribble/eval scribble/base scribble/manual scribble/core
          "utils.rkt" (only-in pict blank))
@(define eval (make-metapict-eval))
@interaction-eval[#:eval eval (require metapict racket/class racket/draw)]
@(define math-style tt)

@title[#:tag "reference-draw-and-fill"]{Drawing and Filling}
@defmodule[metapict/draw]


@; ----------------------------------------
@section[#:tag "ref-draw-and-fill"]{Drawing and Filling}

A @racket[curve] represents the path of a curve. Use @racket[draw]
and @racket[fill] to create a picture in the form of a @racket[pict].
Given a single curve @racket[draw] will use the current pen to
stroke the path and @racket[fill] will use the current brush to fill it.

The size of the pict created by draw and fill functions is determined 
by the parameters @racket[curve-pict-width] and @racket[curve-pict-height].

The position of points, curves etc. are given in logical coordinates.
A pict will only draw the section of the coordinate plane that
is given by the parameter @racket[curve-pict-window]. This parameter
holds the logical window (an x- and y-range) that
will be drawn.

@defproc[(draw [d drawable?] ...) pict?]{
Creates a pict representing an image of the drawable arguments.

Given no arguments a blank pict will be returned.

Given multiple arguments @racket[draw] will convert each argument
into a pict, then layer the results using @racket[cc-superimpose].
In other words: it draw the arguments in order, starting with the first. 

This table shows how the drawable objects are converted:
@(table
  (style #f (list (table-columns (list plain plain plain))))
  (list (list @para{@racket[curve]} @para{@blank[50 1]} @para{@racket[curve->pict]})
        (list @para{@racket[pt]}    @para{ }            @para{@racket[draw-dot]})
        (list @para{@racket[bez]}   @para{ }            @para{@racket[bez->pict]})
        (list @para{@racket[label]} @para{ }            @para{@racket[label->pict]})
        (list @para{@racket[pict]}  @para{ }            @para{@racket[values]})))}

@interaction[#:eval eval 
   (draw (curve (pt -1 0) .. (pt 0 1) .. (pt 1 0))
         (pt 0 0)
         (bez (pt -1/2 0) (pt -1/2 1) (pt 1/2 1) (pt 1/2 0))
         (label-bot "Origo" (pt 0 0)))]

@defproc[(fill [c curve?] ...) pict?]{
Creates a pict that uses the current brush to fill either a single curve 
or to fill areas between curves.

A curve divides the points of the plane in two: the inside and the outside.
The inside is drawn with the brush and the outside is left untouched.

For a simple non-intersecting curve it is simple to decide
whether a point is on the inside or outside. For self-intersecting
curves the so-called winding rule is used. The winding rule is
also used when filling multiple curves

Given a point P consider a ray from P towards infinity. For each
intersection between the ray and the curve(s), determine whether
the curve crosses right-to-left or left-to-right. Each right-to-left
crossing counts as +1 and each left-to-right crossing as -1. If
the total sum of the counts are non-zero, then then point will be
filled.}

Let's look at four concentric circles.
@interaction[#:eval eval 
   (def circles (map circle '(1 3/4 1/2 1/4)))
   (defm (list c1 c2 c3 c4) circles)]
These circles are drawn counter-clockwise. We also need
circles that are drawn clockwise:
@interaction[#:eval eval 
   (defm (list r1 r2 r3 r4) (map curve-reverse circles))]
For a single curve, the orientation doesn't affect the filling:
@interaction[#:eval eval 
     (beside (fill c1) (fill r1))]
For the first filled circle, the winding sum is +1, and
for the second it is -1. Since they are non-zero both
circles are filled.

The four combinations for two circles are:
@interaction[#:eval eval 
(beside (fill c1 c3)
        (fill c1 r3)
        (fill r1 c3)
        (fill r1 r3))]
In the @racket[(fill c1 c3)] case, the points inside @racket[c3] has winding number 2,
and inbetween the circles the winding number is +1. 

For @racket[(fill c1 c3)] the winding numbers are -1+1=0 and +1. Therefore the
points inside c3 are not filled.

Can you figure out the winding numbers in the these three circle cases?
@interaction[#:eval eval 
(beside (fill c2 r3 c4)
        (fill r2 c3 r4))]
And for these:
@interaction[#:eval eval 
(beside (fill c1 r2 c3 r4)
        (fill r1 c2 r3 c4))]
                                      