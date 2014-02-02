#lang racket
(require metapict)

;;; EXAMPLE
;;;   Rotating triangle with shades from yellow to red.
;;;   Inspiration: 
;;;      http://www.texample.net/tikz/examples/rotated-triangle/

(def N 18)

(set-curve-pict-size 500 500)
(with-window (window -0.1 1.1 -0.1 1.1)
  (defv (A B C) (values (pt 0 0) (pt@d 1 60) (pt 1 0)))
  (first-value
   (for/fold ([drawing (draw)] [A A] [B B] [C C]) ([n N])
     (def triangle (curve A -- B -- C -- cycle))
     (def shade    (color-med (expt (/ n N) 0.4) "red" "yellow"))
     (def filled   (color shade (fill triangle)))
     (values (draw drawing filled triangle)
             (med 0.12 A B) (med 0.12 B C) (med 0.12 C A)))))