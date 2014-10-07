#lang scribble/manual
@(require scribble/core scribble/html-properties)
@(require (for-label (except-in racket angle open path? identity ...))
          scribble/extract
          scribble/eval
          scribble/base
          scribble/manual
          "utils.rkt")

@(define eval (make-metapict-eval))
@interaction-eval[#:eval eval (require metapict)]
@(define math-style tt)
@title[#:tag "examples"]{Examples}
@; ----------------------------------------
@section[#:tag "rotating triangle" #:style svg-picts]{Rotating Triangle}

This example was inspired by Alain Matthes's 
@hyperlink["http://www.texample.net/tikz/examples/rotated-triangle/"]{rotated triangle}
TikZ example. 



@interaction[#:eval eval 
(require metapict)
(def N 18)
(set-curve-pict-size 300 300)
(with-window (window -0.1 1.1 -0.1 1.1)
  (defv (A B C) (values (pt 0 0) (pt@d 1 60) (pt 1 0)))
  (first-value
   (for/fold ([drawing (draw)] [A A] [B B] [C C]) ([n N])
     (def triangle (curve A -- B -- C -- cycle))
     (def shade    (color-med (expt (/ n N) 0.4) "red" "yellow"))
     (def filled   (color shade (fill triangle)))
     (values (draw drawing filled triangle)
             (med 0.12 A B) (med 0.12 B C) (med 0.12 C A)))))]




