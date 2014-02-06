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
@title{Examples}
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

@; ----------------------------------------
@section[#:tag "rooty helix" #:style svg-picts]{Rooty Helix}

The example shows the lengths of @racket[sqrt]@math{(n)} for values of @math{n}
from 1 to 86. The design is from Felix Lindemann's 
@hyperlink["http://www.texample.net/tikz/examples/rooty-helix/"]{rooty helix}
TikZ example. 

@interaction[#:eval eval 
(require metapict)

(def max-r 86)

(def dark-green   (make-color* 175 193 36))
(def almost-black (make-color*  50  50 50))

(define (shade r) ; white -> dark-green -> almost-black
  (cond
    [(<= 0 r 1/2) (color-med (* 2 r)         "white"    dark-green   )]
    [(<=   r 1)   (color-med (* 2 (- r 1/2)) dark-green almost-black )]
    [else         (error 'shader (~a "got: " r))]))

(define (spiral drawing max-r)
  (def (node p r)
    (def circle   (circle-curve (pt-x p) (pt-y p) 1.5))
    (def filled   (color "white" (fill circle)))
    (def label    (label-cnt (~a r) p))
    (draw filled circle label))
  (defv (spiral θ)
    (for/fold ([drawing drawing] [θ 0])
              ([r (in-range 1 max-r)])
      (def √r (sqrt r))
      (def (rotθ c) (scaled 4 (rotated θ c)))
      (defv (A B C) (values (pt 0 0) (rotθ (pt √r 0)) (rotθ (pt √r 1))))
      (def triangle (curve A -- B -- C -- cycle))
      (def filled   (color (shade (/ r 86)) (fill triangle)))
      (values (draw drawing filled triangle (node B r))
              (+ θ (acos (sqrt (/ r (+ 1 r))))))))
  (draw spiral 
        (node (scaled 4 (pt@ (sqrt max-r) θ)) max-r)))

(set-curve-pict-size 600 600)
(with-window (window -40 40 -40 40)
  (penwidth 0
    (for/fold ([drawing (draw)]) ([r '(86 38 15)])
      (spiral drawing r))))]

@; ----------------------------------------
@section[#:tag "glider" #:style svg-picts]{Glider - Hacker Emblem}

This figure is a glider, a hacker emblem. The inspiration was
Alex Hirzel @hyperlink["http://www.texample.net/tikz/examples/glider/"]{Glider}.
@interaction[#:eval eval 
(set-curve-pict-size 100 100)
(with-window (window 0 3 0 3)
  (margin 5
    (draw (grid (pt 0 0) (pt 3 3) (pt 0 0) 1)
          (for/draw ([p (list (pt 0 0) (pt 1 0) (pt 2 0) (pt 2 1) (pt 1 2))])
            (fill (circle-curve (pt+ p (vec .5 .5)) 0.42))))))]

