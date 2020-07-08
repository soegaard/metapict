#lang scribble/manual
@(require (for-label metapict (except-in racket angle box open path? unit identity ...))
          scribble/extract scribble/eval scribble/base scribble/manual "utils.rkt")
@(require (only-in metapict .. --))
@(define eval (make-metapict-eval))
@interaction-eval[#:eval eval (require metapict)]
@(define math-style tt)

@title[#:tag "labels"]{Labels}

@defmodule[metapict/label]

@; ----------------------------------------

@section[#:tag "ref-labels"]{Labels}

@defstruct*[label ([string-or-pict string-or-pict?] 
                   [pos            pt?]
                   [plc/vec        placement-or-vec?])]{
This struct represents a label. A label is a string or pict, 
which is drawn next to a certain position @racket[pos].
The placement of the label in relation to the position is
determined by @racket[plc/vec].

If @racket[plc/vec] is the placement @racket[top], the label
is drawn above the position. The provided placements are:
@racket[lft], @racket[ulft], @racket[llft],
@racket[rt], @racket[urt], @racket[lrt],
 @racket[top], @racket[bot], @racket[cnt].

If you need more control over the placment, you can use
a @racket[vec] vektor instead to indicate the direction
from @racket[pos] to the label placement.}

@deftogether[(
  @defstruct*[placement ()]
  @defstruct*[( lft placement) ()]
  @defstruct*[(ulft placement) ()]
  @defstruct*[(llft placement) ()]
  @defstruct*[(  rt placement) ()]
  @defstruct*[( urt placement) ()]
  @defstruct*[( lrt placement) ()]
  @defstruct*[( bot placement) ()]
  @defstruct*[( top placement) ()]
  @defstruct*[( cnt placement) ()])]{
Placements for left, upper left, lower left, right, upper right, lower right,
bottom, top and center respectively.}

@deftogether[(
  @defproc[(label-lft  [str/pict string-or-pict] [pos pt?]) label]
  @defproc[(label-ulft [str/pict string-or-pict] [pos pt?]) label]
  @defproc[(label-llft [str/pict string-or-pict] [pos pt?]) label]
  @defproc[(label-rt   [str/pict string-or-pict] [pos pt?]) label]
  @defproc[(label-urt  [str/pict string-or-pict] [pos pt?]) label]
  @defproc[(label-lrt  [str/pict string-or-pict] [pos pt?]) label]
  @defproc[(label-top  [str/pict string-or-pict] [pos pt?]) label]
  @defproc[(label-bot  [str/pict string-or-pict] [pos pt?]) label]
  @defproc[(label-cnt  [str/pict string-or-pict] [pos pt?]) label])]{
Produces a label at position @racket[pos] where the placement of the label
relative to the position is given from the function name.}

@defproc[(dot-label [str/pict string-or-pict?] [pos pt?] [plc placement-or-vec? (bot)]) pict?]{
Besides placing the string or pict next to the position @racket[pos], also
draw a dot at the position.}

@deftogether[(
  @defproc[(dot-label-lft  [str/pict string-or-pict] [pos pt?]) label]
  @defproc[(dot-label-ulft [str/pict string-or-pict] [pos pt?]) label]
  @defproc[(dot-label-llft [str/pict string-or-pict] [pos pt?]) label]
  @defproc[(dot-label-rt   [str/pict string-or-pict] [pos pt?]) label]
  @defproc[(dot-label-urt  [str/pict string-or-pict] [pos pt?]) label]
  @defproc[(dot-label-lrt  [str/pict string-or-pict] [pos pt?]) label]
  @defproc[(dot-label-top  [str/pict string-or-pict] [pos pt?]) label]
  @defproc[(dot-label-bot  [str/pict string-or-pict] [pos pt?]) label]
  @defproc[(dot-label-cnt  [str/pict string-or-pict] [pos pt?]) label])]{
Same as @racket[dot-label], but the placement is inferred from the name.}


@defproc[(label/offset [str/pict string-or-pict?] [pos pt?] [offset vec?]) label?]{
Like @racket[label], but use @racket[offset] to place the label}

@defproc[(label-bbox [l label?]) curve?]{
Return a curve along the bounding box of the label @racket[l].
Useful for debugging label placements.}

@defproc[(fill-label [fill-color color?] [l label?]) pict?]{
Fill the bounding box of the label with the fill color, then draw the label.
Useful to remove "background noise" below a label.
}

