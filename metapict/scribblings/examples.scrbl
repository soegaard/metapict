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

@section[#:tag "rainbow-circle" #:style svg-picts]{Rainbow Circle}
@interaction[#:eval eval 
(scale (with-window (window -5 5 -5 5)
  (def colors (list "yellow" "orange" "red" "purple" "blue" "green" "yellow"))
    (penwidth 16 
      (margin 20 
        (for*/draw ([θ (in-range 0 2π .01)])
          (def f (/ θ 2π))
          (def c (color-med* f colors))
          (color (change-alpha c (- 1 f))
                 (draw (pt@ 4 θ))))))) 3)]

@section[#:tag "puzzle-missing-square" #:style svg-picts]{Puzzle: The Missing Square}
The two figures are made from the same colored pieces. 
It seems a square is missing from the bottom figure.

Where is it?
@interaction[#:eval eval 
(define red    (curve (pt 0 0) -- (pt 8 0) -- (pt 8 3) -- cycle))
(define blue   (curve (pt 0 0) -- (pt 5 0) -- (pt 5 2) -- cycle))
(define green  (curve (pt 0 0) -- (pt 5 0) -- (pt 5 2) -- (pt 2 2) -- (pt 2 1) -- (pt 0 1) -- cycle))
(define yellow (curve (pt 0 0) -- (pt 2 0) -- (pt 2 1) -- (pt 5 1) -- (pt 5 2) -- (pt 0 2) -- cycle))

(define (draw-pieces positions)
  (for/draw ([p positions]
             [d (list  red   green   yellow   blue)]
             [c (list "red" "green" "yellow" "blue")])
    (def fill-color (change-alpha (color-med 0.2 c "magenta") 0.7))
    (def piece (shifted p d))
    (draw (color fill-color (fill piece))
          piece)))

(set-curve-pict-size 400 (* 13/15 400))
(with-window (window -1 14 -1 12)
  (define upper (list (pt 0 0) (pt 8 0) (pt 8 1) (pt 8 3)))
  (define lower (list (pt 5 2) (pt 8 0) (pt 5 0) (pt 0 0)))
  (margin 2 (draw (color "gray" (draw (grid (pt -1 -1) (pt 14 12) (pt 0 0) 1)))
                  (draw-pieces (map (shifted (pt 0 6)) upper))
                  (draw-pieces lower))))]

@section[#:tag "olympic-rings" #:style svg-picts]{The Olympic Rings}
The inspiration was Paul Gaborit's
@hyperlink["http://www.texample.net/tikz/examples/the-olympic-rings/"]{The Olympic Rings}.
@interaction[#:eval eval 
(struct ring (center color))

(define r1 (ring (pt -4  0)   (make-color*   0 129 188)))
(define r2 (ring (pt -2 -1.8) (make-color* 252 177  49)))
(define r3 (ring (pt  0  0)   (make-color*  35  34  35)))
(define r4 (ring (pt  2 -1.8) (make-color*   0 157  87)))
(define r5 (ring (pt  4  0)   (make-color* 238  50  78)))

(define (draw-rings . rings)
  (for/draw ([r rings])
    (defm (ring p c) r)
    (def c1 (circle-curve p 1.9))
    (def c2 (circle-curve p 1.5))
    (def connector (curve (end-point c1) -- (start-point c2)))
    (def circle-outline (curve-append c1 connector c2))
    (draw (pencolor c (fill circle-outline))
          (penwidth 4 (color "white" (draw c1 c2))))))

(set-curve-pict-size 200 100)
(with-window (window -6 6 -4 2)
  (draw (clipped (draw-rings r5 r4 r3 r2 r1) (box (pt -6  2)   (pt 6 -1.0)))
        (clipped (draw-rings r1 r2 r3 r4 r5) (box (pt -6 -0.8) (pt 6 -3.8)))))]


