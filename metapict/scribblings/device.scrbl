#lang scribble/manual
@(require (for-label metapict (except-in racket angle box open path? unit identity ...))
          #;(for-label metapict/path metapict/path-operations )
          scribble/extract scribble/eval scribble/base scribble/manual "utils.rkt")
@(require (only-in metapict .. --))
@(define eval (make-metapict-eval))
@interaction-eval[#:eval eval (require metapict)]
@(define math-style tt)

@title[#:tag "device"]{Device Settings}

@defmodule[metapict/device]

@; ----------------------------------------

@deftogether[(
  @defparam[curve-pict-width  width  real? #:value 100]
  @defparam[curve-pict-height height real? #:value 100])]{
Parameters used to determine the size of the pict created, when
a curve is converted into a pict by @racket[draw] or similar.

The size is in device coordinates.
}

@deftogether[(
  @defproc[(set-curve-pict-size [w real?] [h real?]) void?]
  @defproc[(set-curve-pict-size [p pict?]) void?])]{
Sets the parameters @racket[curve-pict-width] and @racket[curve-pict-width]
to @racket[w] and @racket[h] or to the width and height of the pict @racket[p].}

@defparam[curve-pict-window win window? #:value (window -1.1 1.1 -1.1 1.1)]{
Parameter that holds the current logical coordinate system in use.
The coordinates of points and vectors use the logical coordinate system.
When drawn, the logical coordinate system is mapped to device coordinates.

For most purposes, it is practical to keep the aspect ratio of the
logical window the same as the device aspect ratio.}

@defproc[(trans-logical-to-device [win window] [device-width real?] [device-height real?]) trans?]{
Computes the affine transformation from the logical coordinate system given by the
window @racket[win] to the window given by @racket[(window 0 device-width 0 device-height)].
}

@defproc[(current-curve-transformation) trans?]{
Computes the transformation from the current window to device coordinates.}

@deftogether[(
  @defproc[(px  [a real?]) real?]
  @defproc[(xpx [a real?]) real?]
  @defproc[(ypx [a real?]) real?])]{
Computes the size of a pixel in logical units.}


@(set! eval #f)



