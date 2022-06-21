#lang scribble/manual
@(require (for-label racket/draw metapict
                     (except-in racket angle box open path? unit identity ...))
          scribble/extract scribble/eval scribble/base scribble/manual "utils.rkt")
@(define eval (make-metapict-eval))
@interaction-eval[#:eval eval (require metapict racket/class racket/draw)]
@(define math-style tt)

@title[#:tag "reference-pict"]{Pict}

@defmodule[metapict/pict]


@; ----------------------------------------
@section[#:tag "ref-pict"]{Pict Adjusters}

All images in MetaPict are represented as @racket[pict]s. A @racket[pict]
is a structure that holds information on how to draw a picture. 
A @racket[pict] can be @emph{rendered} to produce an image in various
formats such as PNG, PDF, and SVG. 

The standard library @racket[pict] defines several functions to
construct and manipulate @racket[pict]s. MetaPict provides and
offers some extra operations. Since they are not MetaPict specific,
they are also useful outside of the world of MetaPict.

A few of the @racket[pict] operations are provided under new names. 
The basic concept in MetaPict is the curve. Therefore it makes sense 
for, say, @racket[circle] to return a curve. In the @racket[pict] library 
the name @tt{circle} returns a @racket[pict], so to avoid a name conflict
it is exported as @racket[circle-pict].

An attempt has been made to make the @racket[pict] the last argument
of all operations. This explains the existance of a few functions whose
functionalities overlap with the @racket[pict] library.

The operations in this section operate on @racket[pict]s, so use
@racket[draw] to convert curves into @racket[pict]s.

@subsection{Pen Adjusters}

@defproc[(color [c color?] [p pict?]) pict?]{
Draws the pict @racket[p] with a solid pen and brush of the color @racket[c].
It is equivalent to @racket[(pencolor c (brushcolor c p))].}
@interaction[#:eval eval (color "red" (filldraw unitcircle))]

@defproc[(pencolor [c color?] [p pict?]) pict?]{
Draws the pict @racket[p] with a solid pen color @racket[c]. 
The brush is not affected by @racket[pencolor].}
@interaction[#:eval eval 
           (penwidth 4
             (beside (pencolor "red" (brushcolor "orange" (filldraw unitcircle)))
                     (pencolor "red" (filldraw unitcircle))))]

@defproc[(penwidth [w real?] [p pict?]) pict?]{
Draws the pict @racket[p] with a pen of width @racket[w], a real number between 0 and 255.
Same as @racket[linewidth].

TODO: What unit?!?!}
@interaction[#:eval eval (apply beside 
                                (for/list ([w 5]) 
                                  (penwidth w (draw unitcircle))))]

@defproc[(penscale [s real?] [p pict?]) pict?]{
Adjusts the current pen width to a width @racket[s] times wider than the 
current, then draws the pict @racket[p].}
@interaction[#:eval eval (beside (penwidth 3 (penscale 2 (draw unitcircle)))
                                 (penscale 3 (penwidth 2 (draw unitcircle)))
                                             (penwidth 6 (draw unitcircle)))]

@defproc[(penstyle [s style?] [p pict?]) pict?]{
Adjusts the current pen style, and then draws the pict @racket[p].
The available styles are: 
@racket['transparent 'solid 'hilite 'dot 'long-dash 'short-dash 'dot-dash ].
Note: The @racket[pen%] documentation mentions a few xor- styles, these
are no longer supported by Racket.}
@interaction[#:eval eval 
(define (styled-circle style) 
  (draw (color "red" (filldraw unitsquare))
        (penstyle style (draw unitcircle))        
        (label-bot (~a style) (pt 0 0))))
(def styles1 '(solid transparent hilite ))
(def styles2 '(dot short-dash long-dash dot-dash))
(above (beside* (map styled-circle styles1))
       (beside* (map styled-circle styles2)))]

@defproc[(pencap [c cap?] [p pict?]) pict?]{
Adjusts the current pen cap, and then draws the pict @racket[p].
The available caps are: @racket['round], @racket['projecting], and, @racket['butt].
The cap determines how the end of curves are drawn.
The default pen is @racket['round].}
@interaction[#:eval eval 
  (define (squiggle cap) 
    (def l (curve (pt -1/2 0) -- (pt 0 0) .. (pt 1/2 1/2)))
    (penwidth 20
      (draw (pencap cap   (color "red"   (draw l)))
            (pencap 'butt (color "black" (draw l)))
            (label-bot (~a cap) (pt 0 -1/2)))))
  (def caps '(round projecting butt))
  (beside* (map squiggle caps))]

@defproc[(penjoin [j join?] [p pict?]) pict?]{
Adjusts the current pen join, and then draws the pict @racket[p].
The available joins are: @racket['round], @racket['bevel], and, @racket['miter].
The join determines how the transition from one curve section to the next is
drawn. The default join is @racket['round].

Note: If you want to draw a rectangle with a crisp 90 degree 
outer angle, then use the @racket['miter] join.}
@interaction[#:eval eval 
  (define (squiggle join) 
    (def l (curve (pt -1/2 0) -- (pt 0 0) .. (pt 1/2 1/2)))
    (draw (penwidth 40 (penjoin join (draw l)))
          (penwidth 2 (color "red" (draw (circle (pt 1/4 -1/3) 1/3))))
          (label-bot (~a join) (pt 0 -1/2))))
  (def joins '(round bevel miter))
  (beside* (map squiggle joins))]

@defproc[(pen [a-pen pen%] [p pict?]) pict?]{
Use the pen @racket[a-pen] as the current pen, then draw the pict @racket[p].}
@interaction-eval[#:eval eval (set-curve-pict-size 100 100)]
@interaction[#:eval eval
  (def teacher-pen
    (new pen% [color "red"]  [width 1]     [style 'solid]
              [cap   'round] [join 'round] [stipple #f]))
  (pen teacher-pen (draw unitcircle))]

@defproc[(dashed [p pict?]) pict]{
Use the pen style @racket['long-dash] to draw the pict @racket[p].}
@interaction[#:eval eval (dashed (draw unitcircle))]

@defproc[(dotted [p pict?]) pict]{
Use the pen style @racket['dot] to draw the pict @racket[p].}
@interaction[#:eval eval (dotted (draw unitcircle))]

@subsection[#:tag "ref-pict-brush-adjusters"]{Brush Adjusters}

@defproc[(brush [b brush%] [p pict?]) pict]{
Use the brush @racket[b] to draw the pict @racket[p].}
@interaction[#:eval eval
  (def hatch (new brush% [color "black"] [style 'crossdiag-hatch]))
  (brush hatch (filldraw unitcircle))]

@defproc[(brushcolor [c color?] [p pict?]) pict]{
Adjust the brush to use the color @racket[b], then draw the pict @racket[p].}
@interaction[#:eval eval
  (brushcolor "red" (fill unitcircle))]

@defproc[(brushstyle [s style?] [p pict?]) pict]{
Adjust the brush to use the style @racket[s], then draw the pict @racket[p].
The example below shows the available styles. The brush style 
@racket[hilite] is black with a 30% alpha.}
@interaction[#:eval eval 
    (define (styled-circle style) 
      (draw (color "red" (filldraw (scaled 0.7 unitsquare)))
            (brushcolor "black" (brushstyle style (fill (scaled 0.7 unitcircle))))
            (brushcolor "white" (draw (label-bot (~a style) (pt 0 -0.7))))))
    (def styles1 '(solid transparent hilite ))
    (def styles2 '(bdiagonal-hatch fdiagonal-hatch crossdiag-hatch))
    (def styles3 '(horizontal-hatch vertical-hatch cross-hatch))
    (above (beside* (map styled-circle styles1))
           (beside* (map styled-circle styles2))
           (beside* (map styled-circle styles3)))]

@defproc[(brushstipple [s style?] [p pict?]) pict]{
Adjust the brush to use the stipple @racket[s], then draw the pict @racket[p].}

@interaction[#:eval eval 
(set-curve-pict-size 256 256)
(define stipple (bitmap "texture.jpeg"))
(with-window (window -1 1 -1 1)
  (beside stipple (blank 64 64)
          (brushstipple (pict->bitmap stipple) 
                        (fill (circle 1)))))]

@defproc[(brushgradient [TODO:TO-BE-DETERMINED gradient?]) pict]{
Use a gradient as brush, then draw the pict @racket[p].}

@defproc[(save-pict [filename path?] [p pict?] [type 'png]) (void)]{
Save the pict @racket[p] as @racket[filename].}
The default format is @racket['png], other formats include @racket['svg],
@racket['pdf], @racket['xbm], @racket['xpm] and @racket['bmp].
JPEG is not included.

@defproc[(margin [r real?] [p pict?]) pict?]{
Equivalent to @racket[(inset p r)].}


@; ----------------------------------------
@subsection{Pict Combiners}

@defproc[(above [p pict?] ...) pict?]{
Draw the picts @racket[p ...] above each other.
If the picts are of different widths, center them.

Same as @racket[vc-append]}

@defproc[(beside [p pict?] ...) pict?]{
Draw the picts @racket[p ...] beside each other.
If the picts are of different heights, center them.

Same as @racket[hc-append]}


@defproc[(above* [ps list?]) pict?]{
Draw the picts in the list @racket[ps] above each other.
The first element is on top. If the picts are of different widths, center them.

Same as @racket[(apply vc-append ps)]}

@defproc[(beside* [ps list?]) pict?]{
Draw the picts in the list @racket[ps] beside each other.
The first element is on the left. If the picts are of different heights, center them.

Same as @racket[(apply hc-append ps)]}


@(set! eval #f)

