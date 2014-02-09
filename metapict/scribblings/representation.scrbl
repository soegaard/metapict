#lang scribble/manual
@(require (for-label metapict (except-in racket angle box open path? identity ...))
          scribble/extract scribble/eval scribble/base scribble/manual "utils.rkt")
@(define eval (make-metapict-eval))
@interaction-eval[#:eval eval (require metapict)]
@(define math-style tt)

@title[#:tag "representation"]{Representation}

@defmodule[metapict/structs]

@local-table-of-contents[]

@; ----------------------------------------
@section[#:tag "ref-bez"]{Representation}

This section describes the representation of the MetaPict concepts.

@defstruct[pt ([x real?] [y real?])]{
The @racket[pt] structure represents a point with coordinates
@math{(x,y)} in the current coordinate system.}
@interaction-eval[#:eval eval (set-curve-pict-size 50 50)]
@interaction[#:eval eval
  (def A (pt 3 4))
  A
  (defm (pt x y) A)
  (list x y)
  (penwidth 4 (draw (pt 0 0) (pt 1/2 1/2) (pt 1 0)))]

@defstruct[vec ([x real?] [y real?])]{
The @racket[vec] structure represents a mathematical vector with coordinates
@math{(x,y)} in the current coordinate system.}
@interaction[#:eval eval
  (def v (vec 3 4))
  v
  (defm (vec x y) v)
  (list x y)
  (def O origo)
  (with-window (window -1 5 -1 5)
    (ahlength (px 5))
    (draw-arrow (curve O -- (pt+ O v))))]

@defstruct[bez ([p0 pt?] [p1 pt?] [p2 pt?] [p3 pt?])]{
The @racket[bez] structure represents a cubic Bezier curve with start point 
in @racket[p0], end point in @racket[p3] and control points in @racket[p1] and @racket[p2].}
@interaction[#:eval eval
  (with-window (window -1 6 -1 6)
    (draw (bez (pt 0 0) (pt 0 1) (pt 2 3) (pt 5 0))))]
