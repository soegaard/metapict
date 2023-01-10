#lang racket/base
;;;
;;; PALETTE
;;;

;; A palette is a function of one argument, a number from 0 to 1,
;; that returns a color.

(provide palette                  ; color ...     -> palette    construct palette from colors
         palette-append           ; palette ...   -> palette    glue palettes together
         draw-curve-with-palette) ; palette curve -> pict       draw curve using a palette


(require metapict racket/match)

(define black (make-color* "black"))
(define black-palette (λ (t) black))


; palette : color ... -> palette
;   Given colors c0, c1, ..., cn returns a palette
;   that returns c0 for t=0 and cn for t=1.
;   Given n=0 colors, the empty palette (constant black)
;   is returned.
(define (palette . colors)
  (match colors
    ['()              black-palette]
    [(list col)       (def c (make-color* col))
                      (λ (t) c)]
    [(list col1 col2) (def c1 (make-color* col1))
                      (def c2 (make-color* col2))
                      (λ (t) (color-med t c1 c2))]
    [(list col ...)   (def cs (list->vector (map make-color* colors)))
                      (def n  (vector-length cs))
                      (λ (t)
                        (def nt (* (- n 1) t))
                        (cond
                          [(integer? nt) (vector-ref cs (inexact->exact nt))]
                          [else          (define i (inexact->exact (floor nt)))
                                         (def c1 (vector-ref cs i))
                                         (def c2 (vector-ref cs (+ i 1)))
                                         (color-med (- nt i) c1 c2)]))]))

(define (palette-append . ps)
  (match ps
    ['()          black-palette]
    [(list p)     (λ (t) (p t))]
    [(list p ...) (def n (length ps))
                  (def v (list->vector ps))
                  (λ (t)
                    (def nt (* n t))
                    (define i (inexact->exact (floor nt)))
                    ((vector-ref v i) (- nt i)))]))

(define (draw-curve-with-palette p c [n 50])
  (def Δt (/ 1 n))
  (def ΔT (/ 1 (- n 1)))
  (for/draw ([t (in-range 0 1 ΔT)])
    (def col (p t))
    (color col (draw (subcurve c t (+ t Δt))))))


#;(begin
    (def c (curve (pt -1 0) -- (pt 1 0)))
    (set-curve-pict-size 401 401)
    (penwidth 3
              (draw-curve-with-palette
               (palette-append (palette "red" "orange")
                               (palette "orange" "yellow" "green" "cyan" "blue"))
               c)))
