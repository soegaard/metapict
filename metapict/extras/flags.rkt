#lang racket
; This module draws flags from the following countries (in alphabetical order):

;    Denmark       flag-denmark

; If your contry is not on the list, lookup the dimensions and offical color tones
; on Wikipedia, write a function and either submit it through GitHub or
; mail jensaxel@soegaard.net to get it included.

(require "../metapict.rkt")

(provide flag-denmark)


(define pantone-186c (make-color* 224 024 054)) ; Dannebrog red


; flag-denmark : -> pict
;  Dannebrog  http://en.wikipedia.org/wiki/Flag_of_Denmark
(define (flag-denmark)
  (define w 37)
  (define h 28)
  (set-curve-pict-size w h)
  (curve-pict-window (window 0 37 0 28))
  ; points at bottom 
  (def row (list (pt 0 0) (pt 12 0) (pt 16 0) (pt 37 0)))
  (defm (list M N O P) (map (shifted 0 28) row))
  (defm (list I J K L) (map (shifted 0 16) row))
  (defm (list E F G H) (map (shifted 0 12) row))
  (defm (list A B C D) row)
  (def red pantone-186c)
  (penjoin 
   'miter 
   (pencap 
    'projecting
    (draw (color "white"  (fill (curve A -- D -- P -- M -- cycle)))
          (color red      (fill (curve A -- B -- F -- E -- cycle)))
          (color red      (fill (curve I -- J -- N -- M -- cycle)))
          (color red      (fill (curve C -- D -- H -- G -- cycle)))
          (color red      (fill (curve K -- L -- P -- O -- cycle)))))))

