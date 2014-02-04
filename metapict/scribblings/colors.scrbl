#lang scribble/manual
@(require (for-label (except-in racket angle open path? identity ...) 
                     metapict metapict/metapict/color)
          scribble/extract scribble/eval scribble/base scribble/manual "utils.rkt")
@(define eval (make-metapict-eval))
@interaction-eval[#:eval eval (require metapict)]
@(define math-style tt)

@defmodule[metapict/color]

@title[#:tag "colors-title"]{Colors}

@local-table-of-contents[]

@; ----------------------------------------
@section[#:tag "colors"]{Colors}

@defproc*[([(make-color* [name string]) color%]
           [(make-color* [r real?] [g real?] [b real?] [α 1.0]) color%])]{
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
                    (color->list blue-ish)
                    (with-window (window 0 1 0 1)
                      (beside (color red-ish  (fill unitsquare))
                              (color purple   (fill unitsquare))))]


