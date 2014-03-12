#lang scribble/manual
@(require (for-label metapict "../pict-lite.rkt" racket/draw
                     (except-in racket angle box open path? identity ...))
          scribble/extract scribble/eval scribble/base scribble/manual "utils.rkt")
@(define eval (make-metapict-eval))
@interaction-eval[#:eval eval (require metapict)]
@(define math-style tt)

@defmodule[metapict/pict]

@title[#:tag "reference-curve"]{Curve}

@; ----------------------------------------
@section[#:tag "ref-curve"]{Curve}

@defproc[(draw [d drawable?] ...) pict?]{
Converts each argument to a pict. The picts are then superimposed with @racket[cc-superimpose].
The first argument becomes the first layer and the last argument becomes the top layer.

If there are no arguments, then @racket[(draw)] returns a blank @racket[pict]
of width @racket[(curve-pict-width)] and height @racket[(curve-pict-width)].

The objects @racket[draw] can render to picts are: @racket[curve], @racket[pict], 
@racket[pt], @racket[bez], and, @racket[label].

TODO: Expand the discussion: describe how each type is converted.
}

@defproc[(draw* [ds (listof drawable?)]) pict?]{
Equivalent to @racket[(apply draw ds)].}

@defproc[(fill [c curve?]) pict?]{
Draws the curve @racket[c] and fills it.

In detail: A new @racket[pict] is created. Then the contents
of the curve is filled with the current brush. The pen is
not used.

The default fill rules is TODO.

The default brush is TODO.
}
@interaction[#:eval eval
    (def triangle (curve (pt 0 0) -- (pt 1/2 1) -- (pt 1 0) -- cycle))
    (beside (fill triangle)
            (pencolor "red" (brushcolor "pink" (fill triangle))))]


@defproc[(filldraw [c curve?]) pict?]{
Draws, then fills, the curve @racket[c]. 
                             
That is, @racket[filldraw] is equivalent to @racket[(draw (fill c))]. 
}
@interaction[#:eval eval
    (def triangle (curve (pt 0 0) -- (pt 1/2 1) -- (pt 1 0) -- cycle))
    (beside (filldraw triangle)
            (pencolor "red" (brushcolor "pink" (filldraw triangle))))]
@interaction[#:eval eval
    (def triangle (curve (pt 0 0) -- (pt 1/2 1) -- (pt 1 0) -- cycle))
    (pencolor "red" (brushcolor "pink" (filldraw triangle)))]
@interaction[#:eval eval
    (def triangle (curve (pt 0 0) -- (pt 1/2 1) -- (pt 1 0) -- cycle))
    (draw (brushcolor "pink" (fill  triangle))
          (pencolor "red" (draw triangle)))]
@interaction[#:eval eval
    (def triangle (curve (pt 0 0) -- (pt 1/2 1) -- (pt 1 0) -- cycle))
    (draw (brushcolor "pink" (fill  triangle))
          (color "red" (draw triangle)))]


@defproc[(clipped [p pict?] [c curve?]) pict?]{
Draws the parts of the pict @racket[p] inside the curve @racket[c].
}
@interaction[#:eval eval
   (def c unitcircle)
   (def p (grid (pt -1 -1) (pt 1 1) (pt 0 0) 1/3))
   (beside p (draw c) (clipped p c))]







