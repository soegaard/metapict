#lang scribble/manual
@(require (for-label metapict pict racket/draw
                     (except-in racket angle box open path? identity ...))
          scribble/extract scribble/eval scribble/base scribble/manual "utils.rkt")
@(define eval (make-metapict-eval))
@interaction-eval[#:eval eval (require metapict)]
@(define math-style tt)

@defmodule[metapict/pict]

@; ----------------------------------------
@section[#:tag "ref-pict"]{Pict Adjusters}

All images in MetaPict are represented as @racket[pict]s. A @racket[pict]
is a structure that holds information on how to draw a picture. 
A @racket[pict] can be @emph{rendered} to produce an image in various
formats such as png, pdf, and, svg. 

The standard library @racket[pict] defines lots of functions to
construct and manipulate @racket[pict]s. MetaPict provides and
offer a some extra operations. Since they are not MetaPict specific,
they are also useful outside the world of MetaPict.

A few of the @racket[pict] operations are provided under new names. 
The basic concept in MetaPict is the curve. Therefore it makes sense 
for, say, @racket[circle] to return a curve. In the @racket[pict] library 
the name @tt{circle} returns a @racket[pict], so to avoid a name conflict
it is exported as @racket[circle-pict].

An attempt have been made to make the @racket[pict] the last argument
of all operations. This explains the existance of a few functions whose
functionality overlap with the @racket[pict] library.

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
                    (beside (pencolor "red" (brushcolor "orange" (filldraw unitcircle)))
                            (pencolor "red" (filldraw unitcircle)))]

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

Note: If you want to draw a rectangle with crisp 90 degree 
      outer angle, then use the @racket['miter] join.}
@interaction[#:eval eval 
  (define (squiggle join) 
    (def l (curve (pt -1/2 0) -- (pt 0 0) .. (pt 1/2 1/2)))
    (draw (penwidth 40 (penjoin join (draw l)))
          (penwidth 2 (color "red" (draw (circle-curve (pt 1/4 -1/3) 1/3))))
          (label-bot (~a join) (pt 0 -1/2))))
  (def joins '(round bevel miter))
  (beside* (map squiggle joins))]

@defproc[(pen [a-pen pen%] [p pict?]) pict?]{
Use the pen @racket[a-pen] as the current pent, then draw the pict @racket[p].}
@interaction-eval[#:eval eval (set-curve-pict-size 100 100)]
@interaction[#:eval eval
  (def teacher-pen
    (new pen% [color "red"]  [width 1]     [style 'solid]
              [cap   'round] [join 'round] [stipple #f]))
  (pen teacher-pen (draw unitcircle))]

@defproc[(dashed [p pict?]) pict]{
Use the pen style @racket['long-dash] to draw the pict @racket[p]}

@defproc[(dotted [p pict?]) pict]{
Use the pen style @racket['dot] to draw the pict @racket[p]}

@subsection[#:tag "ref-pict-brush-adjusters"]{Brush Adjusters}

TODO: Fill in these.

@defproc[(brush [b brush%] [p pict?]) pict]{
Use the brush @racket[b] to draw the pict @racket[p]}
@defproc[(brushcolor [c color?] [p pict?]) pict]{
Adjust the brush with to use the color @racket[b], then draw the pict @racket[p]}
@defproc[(brushstyle [s style?] [p pict?]) pict]{
Adjust the brush style to use the style @racket[s], then draw the pict @racket[p]}
@defproc[(brushstipple [s style?] [p pict?]) pict]{
Adjust the brush stipple to use the stipple @racket[s], then draw the pict @racket[p]}
@defproc[(brushgradient [TODO:TO-BE-DETERMINED gradient?]) pict]{
Use a gradient as brush, then draw the pict @racket[p]}
@defproc[(save-pict-as-png [filename path?] [p pict?]) (void)]{
Save the pict @racket[p] as a png-file named  @racket[filename].}
@defproc[(margin [r real?] [p pict?]) pict?]{
Equivalent to @racket[(inset p r)].}


