#lang info

(define scribblings '(("scribblings/metapict.scrbl" ())))
(define compile-omit-paths 
  '("todo"     ; this contains unfinished bits and pieces
    "exp"      ; experiments written to see how stuff works
    "examples"
    ))

(define deps '("base"
               "draw-lib"
               "math-lib"
               "gui-lib"
               "parser-tools-lib"
               "pict-lib"
               "slideshow-lib"
               "srfi-lite-lib"
               "ppict"
               "htdp-lib"
               "compatibility-lib"
               "graph-lib"
               "plot-gui-lib"
               "plot-lib"
               "rackunit-lib"
               "racket-poppler"
               "unstable-latent-contract-lib"
               "unstable-parameter-group-lib"
               "at-exp-lib" "rackunit-lib"))

(define build-deps '( "scribble-lib" "racket-doc" "draw-doc" "pict-doc"
                                     "racket-poppler"))
