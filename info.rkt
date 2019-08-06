#lang info
(define collection 'multi)
(define version    "0.1")
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
               "unstable-latent-contract-lib"
               "unstable-parameter-group-lib"
               "at-exp-lib" "rackunit-lib"))


(define build-deps '( "scribble-lib"
                     "racket-doc" "draw-doc" "pict-doc"))

