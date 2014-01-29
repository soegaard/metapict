#lang scribble/manual
@(require (for-label (except-in racket angle open path? identity ...)
                     metapict)
          scribble/extract
          scribble/eval
          scribble/base
          scribble/manual
          "utils.rkt")

@(define eval (make-metapict-eval))
@interaction-eval[#:eval eval (require metapict)]
@(define math-style tt)

@title[#:tag "points"]{Points (pt)}
@(author-jens-axel)

@defmodule[metapict/pt]

@local-table-of-contents[]

@; ----------------------------------------
@section[#:tag "points"]{Points}

Points are represented as @racket[pt] structures.
