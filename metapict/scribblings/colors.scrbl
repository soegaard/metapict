#lang scribble/manual
@(require (for-label (except-in racket angle open path? identity ...) 
                     metapict/metapict/color)
          scribble/extract scribble/eval scribble/base scribble/manual "utils.rkt")
@(define eval (make-metapict-eval))
@interaction-eval[#:eval eval (require metapict)]
@(define math-style tt)

@title[#:tag "colors-title"]{Colors}

@defmodule[metapict/metapict/color]

@local-table-of-contents[]

@; ----------------------------------------
@section[#:tag "colors"]{Colors}

@defproc*[([(make-color* [name string?]) (is-a?/c color%)]
           [(make-color* [r real?] [g real?] [b real?] [α 1.0]) (is-a?/c color%)])]{
The function @racket[make-color*] is a fault tolerant version of @racket[make-color]
that also accepts color names.
                                                                         
Given a color name as a string, @racket[make-color*] returns a @racket[color%] object.

Given real numbers to use as the color components, @racket[make-color*] works
like @racket[make-color], but accepts both non-integer numbers, and 
numbers outside the range 0--255. For a real number @racket[x] the value
used is @racket[(min 255 (max 0 (exact-floor x)))].

The optional argument @racket[α] is the transparency. The default value is 1.
Given a transparency outside the interval 0--1 whichever value of 0 and 1 is
closest to @racket[α] is used.}
@interaction-eval[#:eval eval (set-curve-pict-size 50 50)] 
@interaction[#:eval eval 
                    (def red-ish  (make-color* 300 -12 42.3))
                    (def purple   (make-color* "purple"))
                    (color->list red-ish)
                    (color->list purple)
                    (with-window (window 0 1 0 1)
                      (beside (color red-ish  (fill unitsquare))
                              (color purple   (fill unitsquare))))]
@deftogether[(@defproc*[([(color [c (is-a?/c color%)] [p pict?]) pict?]
                         [(color [f real?] [c (is-a?/c color%)] [p pict?]) pict?])])
             #;(@defthing[([(color r g b a)])])]{
In an expression context @racket[(color c p)] is equivalent to @racket[(colorize p c)]
and @racket[(color f c p)] is equivalent to @racket[(colorize p (color* f c))].

As a match pattern @racket[(color r g b a)] matches both @racket[color%] objects 
and color names (represented as strings). The variables @racket[r], 
@racket[g], and, @racket[b] will be bound to the red, green, and, blue components
of the color. The variable @racket[a] will be bound to the transparency.}

@interaction-eval[#:eval eval (set-curve-pict-size 50 50)] 
@interaction[#:eval eval 
                    (with-window (window 0 1 0 1)
                      (apply beside (for/list ([f (in-range 0 1.1 .1)])
                                      (color f "red" (fill unitsquare)))))
                    (require racket/match)
                    (match "magenta"
                      [(color r g b a) (list r g b a)])]

@defproc[(color->list [c color]) (listof real?)]{
Returns a list of the color components and the transparency of the color @racket[c].
The color can be a @racket[color%] object or a color name (string).}

@interaction[#:eval eval (color->list "magenta")]

@defproc[(color+ [c1 color] [c2 color]) (is-a?/c color%)]{
Returns a @racket[color%] objects, whose color components are
the components of @racket[c1] and @racket[c2] added componentwise.
The transparency is @racket[(min 1.0 (+ α1 α2))] where 
@racket[α1] and @racket[α2] the transparencies of the two colors.}

@interaction-eval[#:eval eval (set-curve-pict-size 50 50)] 
@interaction[#:eval eval (color->list (color+ "red" "blue"))]


@defproc[(color* [k real?] [c color]) (is-a?/c color%)]{
Returns a @racket[color%] object, whose color components are the 
components of @racket[c] multiplied componentwise with @racket[k].
The transparency is the same as in @racket[c].}

@interaction-eval[#:eval eval (set-curve-pict-size 50 50)] 
@interaction[#:eval eval (color->list (color* 0.5 "blue"))]


@defproc[(color-med [t real?] [c1 color] [c2 color]) (is-a?/c color%)]{
Interpolates linearly between the colors @racket[c1] and @racket[c2].
For @math["t=0"] the color @racket[c1] is returned, and when 
@math["t=1"] the color @racket[c2] is returned.}

@interaction-eval[#:eval eval (set-curve-pict-size 50 50)] 
@interaction[#:eval eval (with-window (window 0 1 0 1)
                           (apply beside (for/list ([t (in-range 0 1.1 .1)])
                                           (color (color-med t "red" "yellow")
                                                  (fill unitsquare)))))]

@defproc[(color-med* [t real?] [cs (listof color)]) (is-a?/c color%)]{
Interpolates linearly between the colors in the list @racket[cs].
For @math{"t=0"} corresponds to the first color in the list,
and @math{"t=1"} corresponds to the last color.}

@interaction-eval[#:eval eval (set-curve-pict-size 50 50)]
@interaction[#:eval eval (with-window (window 0 1 0 1)
                           (apply beside (for/list ([t (in-range 0 11/10 1/10)])
                                           (color (color-med* t '("red" "yellow" "blue" "green"))
                                                  (fill unitsquare)))))]

@deftogether[( @defproc[(change-red   [c color] [r real?]) (is-a?/c color%)]
               @defproc[(change-blue  [c color] [r real?]) (is-a?/c color%)]
               @defproc[(change-green [c color] [r real?]) (is-a?/c color%)]
               @defproc[(change-alpha [c color] [r real?]) (is-a?/c color%)])]{
Returns a color objects like @racket[c] where one component has been changed to @racket[r].}

@interaction[#:eval eval (color->list (change-red "blue" 42))]




