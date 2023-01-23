#lang racket
;;;
;;; Chess
;;;

;; This file contains chess related pict functions.
;; The CBurnett chess pieces from Wikipedia/Lichess have
;; been converted from SVG format to picts.

(require "svg-path.rkt"
         racket/draw
         (prefix-in pict: pict))
;;;
;;; Settings
;;;

(define current-piece-width  (make-parameter 45))
(define current-piece-height (make-parameter 45))

;;;
;;; Color Helpers
;;;

; svg-color : string -> color%
(define (svg-color str)
  (define (hex x) (string->number x 16))
  (cond
    [(char=? (string-ref str 0) #\#)
     (svg-color (substring str 1 (string-length str)))]
    [else
     (match (regexp-match "(..)(..)(..)" str)
       [(list rgb r g b)
        (make-object color% (hex r) (hex g) (hex b))]
       [_
        (error 'svg-color "expected \"rrggbb\" where r, g and b are hexadecimal, got: ~a" str)])]))

; solid-brush : color/color-string -> brush%
(define (solid-brush color)
  (define col
    (if (and (string? color)
             (char=? (string-ref color 0) #\#))
        (svg-color color)
        color))
  (new brush% [color col] [style 'solid]))

;;;
;;; Brushes
;;;

(define transparent-brush   (new brush% [color "black"] [style 'transparent] ))
(define black-brush            (solid-brush "black"))
(define white-brush            (solid-brush "white"))
(define light-brush            (solid-brush "#EFDAB5"))
(define dark-brush             (solid-brush "#B58863"))
(define moved-brush            (solid-brush "#ACA249"))  ; background for last moved piece
(define black-decoration-brush (solid-brush "#ececec"))  ; for decorating black pieces
(define white-decoration-brush (solid-brush "#000000"))  ; for decorating white pieces
  
(define transparent-pen           (new pen%   [style 'transparent] [width 0]))
(define black-pen-1.5-butt        (new pen%   [color "black"] [width 1.5] [cap 'butt]  [join 'round]))
(define white-pen-1.5-butt/miter  (new pen%   [color "white"] [width 1.5] [cap 'butt]  [join 'miter]))
(define black-pen-1.5-butt/miter  (new pen%   [color "black"] [width 1.5] [cap 'butt]  [join 'miter]))
(define white-pen-1.5-round/miter (new pen%   [color "white"] [width 1.5] [cap 'round] [join 'miter]))
(define black-pen-1.5-round/miter (new pen%   [color "black"] [width 1.5] [cap 'round] [join 'miter]))
(define white-pen-1.5-round       (new pen%   [color "white"] [width 1.5] [cap 'round] [join 'round]))
(define black-pen-1.5-round       (new pen%   [color "black"] [width 1.5] [cap 'round] [join 'round]))
(define outline-pen black-pen-1.5-round)
(define white-decoration-pen      (new pen%   [color "black"]               [width 1.5] [cap 'round] [join 'round]))
(define black-decoration-pen      (new pen%   [color (svg-color "#ececec")] [width 1.5] [cap 'round] [join 'round]))

(define current-light-brush (make-parameter light-brush))
(define current-dark-brush  (make-parameter dark-brush))

;;;
;;; Path Helpers
;;;

(define (draw-paths dc paths [xoffset 0] [yoffset 0] [fillstyle 'odd-even])
  (for ([path paths])
    (send dc draw-path path xoffset yoffset fillstyle)))

;;;
;;; Extended version of `filled-rectangle` that accepts pen and brush as arguments.
;;;
(define (filled-rectangle w h
                            #:draw-border? [draw-border? #t]
                            #:color        [color #f]
                            #:border-color [border-color #f]
                            #:border-width [border-width #f]
                            #:brush        [brush #f]
                            #:pen          [pen #f])
    (draw-shape/border w h (lambda (dc x y) (send dc draw-rectangle x y w h))
                       color border-color border-width
                       #:draw-border? draw-border?
                       #:brush brush #:pen pen))

(define (draw-shape/border w h draw-fun
                             color [border-color #f] [border-width #f]
                             #:draw-border? [draw-border? #t]
                             #:transparent? [transparent? #f]
                             #:brush        [brush        #f]
                             #:pen          [pen          #f])
    (pict:dc (λ (dc dx dy)
          (define old-brush (send dc get-brush))
          (define old-pen   (send dc get-pen))
          (send dc set-brush
                (or brush
                    (send the-brush-list find-or-create-brush
                      (cond [transparent? "white"]
                            [color        color]
                            [else         (send old-pen get-color)])
                      (if transparent? 'transparent 'solid))))
          (if draw-border?
              (when (or border-color border-width pen)
                ;; otherwise, leave pen as is
                (send dc set-pen (or pen
                                     (send the-pen-list
                                           find-or-create-pen
                                           (or border-color
                                               (send old-pen get-color))
                                           (or border-width
                                               (send old-pen get-width))
                                           (send old-pen get-style)))))
              (send dc set-pen "black" 1 'transparent))
          (draw-fun dc dx dy)
          (send dc set-brush old-brush)
          (send dc set-pen   old-pen))
        w h))


;;;
;;; Picts for single pieces.
;;;

;; The piece drawing functions below draw the piece in size 45x45.
;; Use this function to convert a drawing function into a pict of size wxh.
(define (make-chess-pict draw-it w h)
  ; note: draw-it draws the piece in the size 45x45
  ; draw : (-> (is-a?/c dc<%>) real? real? any)
  (define (draw dc dx dy)
    (define-values (old-scale-x old-scale-y) (send dc get-scale))
    (define old-brush          (send dc get-brush))
    (define old-pen            (send dc get-pen))
    (define old-smoothing      (send dc get-smoothing))
    (define old-transformation (send dc get-transformation))
    (send dc translate dx dy)
    (send dc set-scale (/ w 45) (/ h 45))
    (send dc set-smoothing 'smoothed)
    (draw-it dc)
    (send dc set-smoothing old-smoothing)
    (send dc set-scale old-scale-x old-scale-y)
    (send dc translate (- dx) (- dy))
    (send dc set-transformation old-transformation)
    (send dc set-brush old-brush)
    (send dc set-pen old-pen))
  (pict:dc draw w h))

;;;
;;; Convenient Shorthand for single pieces.
;;; 

(define (bB [w (current-piece-width)] [h (current-piece-height)])  (make-chess-pict black-bishop w h))
(define (wB [w (current-piece-width)] [h (current-piece-height)])  (make-chess-pict white-bishop w h))
(define (bN [w (current-piece-width)] [h (current-piece-height)])  (make-chess-pict black-knight w h))
(define (wN [w (current-piece-width)] [h (current-piece-height)])  (make-chess-pict white-knight w h))
(define (bK [w (current-piece-width)] [h (current-piece-height)])  (make-chess-pict black-king w h))
(define (wK [w (current-piece-width)] [h (current-piece-height)])  (make-chess-pict white-king w h))
(define (bQ [w (current-piece-width)] [h (current-piece-height)])  (make-chess-pict black-queen w h))
(define (wQ [w (current-piece-width)] [h (current-piece-height)])  (make-chess-pict white-queen w h))
(define (bP [w (current-piece-width)] [h (current-piece-height)])  (make-chess-pict black-pawn w h))
(define (wP [w (current-piece-width)] [h (current-piece-height)])  (make-chess-pict white-pawn w h))
(define (bR [w (current-piece-width)] [h (current-piece-height)])  (make-chess-pict black-rook w h))
(define (wR [w (current-piece-width)] [h (current-piece-height)])  (make-chess-pict white-rook w h))


(define (piece-bitmap piece [light? #f] #:size [size 200])
  ; The pieces assume they are drawn on a 45x45 tile,
  ; so we need to scale the x- and y-coordinates.
  (define scale (/ size 45))  
  (define bm (make-object bitmap% size size))
  (define dc (new bitmap-dc% [bitmap bm]))
  (send dc set-scale scale scale)
  (send dc set-smoothing 'smoothed)
  ; background
  (send dc set-brush (if light? (current-light-brush) (current-dark-brush)))
  (send dc set-pen   transparent-pen)
  (send dc draw-rectangle -10 -10 (+ size 10) (+ size 10))
  ; draw piece
  (piece dc)
  ; return bitmap
  bm)

(define (black-bishop dc)
  (define decoration-pen (new pen%   [color (svg-color "#ececec")] [width 1] [cap 'round] [join 'miter]))
  (draw-bishop dc
               black-brush
               black-pen-1.5-butt
               black-pen-1.5-round/miter
               decoration-pen))

(define (white-bishop dc)
  (define decoration-pen (new pen%   [color "black"] [width 1] [cap 'round] [join 'miter]))
  (draw-bishop dc
               white-brush
               black-pen-1.5-butt
               black-pen-1.5-round/miter
               decoration-pen))
  

(define (draw-bishop dc brush pen-butt pen-round/miter decoration-pen)
  (define p1 (parse-svg-path (~a "M9 36c3.39-.97 10.11.43 13.5-2 3.39 2.43 10.11 1.03 13.5 2 "
                                 "0 0 1.65.54 3 2-.68.97-1.65.99-3 .5-3.39-.97-10.11.46-13.5-1-3.39 "
                                 "1.46-10.11.03-13.5 1-1.354.49-2.323.47-3-.5 1.354-1.94 3-2 3-2z")))
  (define p2 (parse-svg-path (~a "M15 32c2.5 2.5 12.5 2.5 15 0 .5-1.5 0-2 0-2 0-2.5-2.5-4-2.5-4 "
                                 "5.5-1.5 6-11.5-5-15.5-11 4-10.5 14-5 15.5 0 0-2.5 1.5-2.5 4 0 0-.5.5 0 2z")))
  (define p3 (parse-svg-path     "M25 8a2.5 2.5 0 1 1-5 0 2.5 2.5 0 1 1 5 0z"))
  (define p4 (parse-svg-path     "M17.5 26h10M15 30h15m-7.5-14.5v5M20 18h5"))

  (send dc set-brush brush)
  (send dc set-pen   pen-butt)
  (draw-paths dc (svg-path->paths p1) 0 0 'odd-even)
  (draw-paths dc (svg-path->paths p2) 0 0 'odd-even)
  (draw-paths dc (svg-path->paths p3) 0 0 'odd-even)
  ; decorations
  (send dc set-brush transparent-brush)
  (send dc set-pen   decoration-pen)
  (draw-paths dc (svg-path->paths p4)))


(define (draw-pawn dc brush outline-pen)
  (define p1 (parse-svg-path
              (~a "M22.5 9c-2.21 0-4 1.79-4 4 0 .89.29 1.71.78 2.38C17.33 16.5 16 18.59 16 21"
                  "c0 2.03.94 3.84 2.41 5.03-3 1.06-7.41 5.55-7.41 13.47h23c0-7.92-4.41-12.41-7.41-13.47"
                  " 1.47-1.19 2.41-3 2.41-5.03 0-2.41-1.33-4.5-3.28-5.62.49-.67.78-1.49.78-2.38 0-2.21-1.79-4-4-4z")))
  (send dc set-pen   outline-pen)
  (send dc set-brush brush)
  (draw-paths dc (svg-path->paths p1) 0 0 'odd-even))
(define (black-pawn dc)
  (draw-pawn dc black-brush black-pen-1.5-round))
(define (white-pawn dc)
  (draw-pawn dc white-brush black-pen-1.5-round))

;; The rooks use different paths, so we have two versions.
(define (draw-rook/black dc brush pen-butt/round pen-butt/miter pen-decoration-round/miter)
  (define p1 (parse-svg-path "M9 39h27v-3H9v3zm3.5-7l1.5-2.5h17l1.5 2.5h-20zm-.5 4v-4h21v4H12z"))
  (define p2 (parse-svg-path "M14 29.5v-13h17v13H14z"))
  (define p3 (parse-svg-path "M14 16.5L11 14h23l-3 2.5H14zM11 14V9h4v2h5V9h5v2h5V9h4v5H11z"))
  (define p4 (parse-svg-path "M12 35.5h21m-20-4h19m-18-2h17m-17-13h17M11 14h23"))

  (send dc set-brush brush)
  (send dc set-pen   pen-butt/round)  
  (draw-paths dc (svg-path->paths p1) 0 0 'odd-even)
  (send dc set-pen   pen-butt/miter)
  (draw-paths dc (svg-path->paths p2) 0 0 'odd-even)  
  (send dc set-pen   pen-butt/round)  
  (draw-paths dc (svg-path->paths p3) 0 0 'odd-even)
  (send dc set-pen   pen-decoration-round/miter)  
  (draw-paths dc (svg-path->paths p4) 0 0 'odd-even))

(define (draw-rook/white dc brush pen-butt/round pen-butt/miter pen-round/round  pen-decoration-round/miter)
  (define p1 (parse-svg-path "M9 39h27v-3H9v3zm3-3v-4h21v4H12zm-1-22V9h4v2h5V9h5v2h5V9h4v5"))
  (define p2 (parse-svg-path "M34 14l-3 3H14l-3-3"))
  (define p3 (parse-svg-path "M31 17v12.5H14V17"))
  (define p4 (parse-svg-path "M31 29.5l1.5 2.5h-20l1.5-2.5"))
  (define p5 (parse-svg-path "M11 14h23"))
  ; fill
  (send dc set-brush brush)
  (send dc set-pen   pen-butt/round)  
  (draw-paths dc (svg-path->paths p1) 0 0 'odd-even)
  (send dc set-pen   pen-round/round)
  (draw-paths dc (svg-path->paths p2) 0 0 'odd-even)  
  (send dc set-pen   pen-butt/miter)  
  (draw-paths dc (svg-path->paths p3) 0 0 'odd-even)
  (send dc set-pen   pen-round/round)  
  (draw-paths dc (svg-path->paths p4) 0 0 'odd-even)  
  ; no fill
  (send dc set-brush transparent-brush)
  (send dc set-pen   pen-decoration-round/miter)  
  (draw-paths dc (svg-path->paths p5) 0 0 'odd-even))

(define (black-rook dc)
  (define pen-decoration (new pen%   [color (svg-color "#ececec")] [width 1] [cap 'round] [join 'miter]))
  (draw-rook/black dc black-brush
                   black-pen-1.5-butt black-pen-1.5-butt/miter
                   pen-decoration))

(define (white-rook dc)
  (define pen-decoration (new pen% [color "black"] [width 1] [cap 'round] [join 'miter]))
  (draw-rook/white dc
                   white-brush
                   black-pen-1.5-butt
                   black-pen-1.5-butt/miter
                   black-pen-1.5-round
                   pen-decoration))
  

(define (black-knight dc)
  (define pen-decoration   (new pen% [color (svg-color "#ececec")] [width 1.5] [cap 'round] [join 'round]))
  (draw-knight dc
               black-brush         black-decoration-brush 
               black-pen-1.5-round pen-decoration))

(define (white-knight dc)
  (define mane? #f)
  (draw-knight dc
               white-brush         white-decoration-brush 
               black-pen-1.5-round white-decoration-pen
               mane?))

(define (draw-knight dc brush decoration-brush pen-round pen-decoration [mane? #t])
  (define p1 (parse-svg-path     "M22 10c10.5 1 16.5 8 16 29H15c0-9 10-6.5 8-21"))
  (define p2 (parse-svg-path (~a "M24 18c.38 2.91-5.55 7.37-8 9-3 2-2.82 4.34-5 4-1.042-.94 "
                                 "1.41-3.04 0-3-1 0 .19 1.23-1 2-1 0-4.003 1-4-4 0-2 6-12 "
                                 "6-12s1.89-1.9 2-3.5c-.73-.994-.5-2-.5-3 1-1 3 2.5 3 "
                                 "2.5h2s.78-1.992 2.5-3c1 0 1 3 1 3")))
  (define p3 (parse-svg-path (~a "M9.5 25.5a.5.5 0 1 1-1 0 .5.5 0 1 1 1 0z"
                                 "m5.433-9.75a.5 1.5 30 1 1-.866-.5.5 1.5 30 1 1 .866.5z")))
  (define p4 (parse-svg-path (~a "M24.55 10.4l-.45 1.45.5.15c3.15 1 5.65 2.49 7.9 6.75S35.75 29.06 35.25 39"
                                 "l-.05.5h2.25l.05-.5c.5-10.06-.88-16.85-3.25-21.34-2.37-4.49-5.79-6.64-9.19-7.16l-.51-.1z")))
  ; fill
  (send dc set-brush brush)
  (send dc set-pen   pen-round)
  (draw-paths dc (svg-path->paths p1) 0 0 'odd-even)  
  (draw-paths dc (svg-path->paths p2) 0 0 'odd-even)
  ; decoration fill
  (send dc set-brush decoration-brush)
  (send dc set-pen   pen-decoration)
  (draw-paths dc (svg-path->paths p3) 0 0 'odd-even)
  (send dc set-pen   transparent-pen)
  (when mane?
    (draw-paths dc (svg-path->paths p4) 0 0 'odd-even)))

(define (black-king dc)
  (draw-king/black dc
                   black-brush
                   black-pen-1.5-round/miter
                   black-pen-1.5-butt/miter
                   black-pen-1.5-round
                   black-decoration-pen))

(define (white-king dc)
  (draw-king/white dc
                   white-brush
                   black-pen-1.5-round/miter ; cross
                   black-pen-1.5-butt/miter
                   black-pen-1.5-round
                   white-decoration-pen))

(define (draw-king/black dc brush pen-round/miter pen-butt/miter pen-round/round decoration-pen)
  ; When the outline and fill color is the same, an extra path just inside the outline is drawn.
  ; So the black king is slightly more complicated than the white king.
  (define p1 (parse-svg-path "M22.5 11.63V6"))
  (define p2 (parse-svg-path "M22.5 25s4.5-7.5 3-10.5c0 0-1-2.5-3-2.5s-3 2.5-3 2.5c-1.5 3 3 10.5 3 10.5"))
  (define p3 (parse-svg-path "M11.5 37c5.5 3.5 15.5 3.5 21 0v-7s9-4.5 6-10.5c-4-6.5-13.5-3.5-16 4V27v-3.5c-3.5-7.5-13-10.5-16-4-3 6 5 10 5 10V37z"))
  (define p4 (parse-svg-path "M20 8h5"))
  (define p5 (parse-svg-path "M32 29.5s8.5-4 6.03-9.65C34.15 14 25 18 22.5 24.5l.01 2.1-.01-2.1C20 18 9.906 14 6.997 19.85c-2.497 5.65 4.853 9 4.853 9"))
  (define p6 (parse-svg-path "M11.5 30c5.5-3 15.5-3 21 0m-21 3.5c5.5-3 15.5-3 21 0m-21 3.5c5.5-3 15.5-3 21 0"))  
  ; fill
  (send dc set-brush brush)
  (send dc set-pen   pen-round/miter)
  (draw-paths dc (svg-path->paths p1) 0 0 'odd-even)
  (send dc set-pen   pen-butt/miter)
  (draw-paths dc (svg-path->paths p2) 0 0 'odd-even)
  (send dc set-pen   pen-round/round)
  (draw-paths dc (svg-path->paths p3) 0 0 'odd-even)
  ; no fill
  (send dc set-brush transparent-brush)
  (send dc set-pen   pen-round/miter)
  (draw-paths dc (svg-path->paths p4) 0 0 'odd-even)
  (send dc set-pen   decoration-pen)
  (draw-paths dc (svg-path->paths p5) 0 0 'odd-even)
  (draw-paths dc (svg-path->paths p6) 0 0 'odd-even))

(define (draw-king/white dc brush pen-round/miter pen-butt/miter pen-round/round decoration-pen)
  (define p1 (parse-svg-path "M22.5 11.63V6M20 8h5"))
  (define p2 (parse-svg-path "M22.5 25s4.5-7.5 3-10.5c0 0-1-2.5-3-2.5s-3 2.5-3 2.5c-1.5 3 3 10.5 3 10.5" ))
  (define p3 (parse-svg-path "M11.5 37c5.5 3.5 15.5 3.5 21 0v-7s9-4.5 6-10.5c-4-6.5-13.5-3.5-16 4V27v-3.5c-3.5-7.5-13-10.5-16-4-3 6 5 10 5 10V37z"))
  (define p4 (parse-svg-path "M11.5 30c5.5-3 15.5-3 21 0m-21 3.5c5.5-3 15.5-3 21 0m-21 3.5c5.5-3 15.5-3 21 0"))
  ; fill
  (send dc set-brush brush)
  (send dc set-pen   pen-round/miter)
  (draw-paths dc (svg-path->paths p1) 0 0 'odd-even)
  (send dc set-pen   pen-butt/miter)
  (draw-paths dc (svg-path->paths p2) 0 0 'odd-even)  
  (send dc set-brush brush)  
  (send dc set-pen   pen-round/round)  
  (draw-paths dc (svg-path->paths p3) 0 0 'odd-even)
  ; no fill
  (send dc set-brush transparent-brush)
  (send dc set-pen   pen-round/round)
  (draw-paths dc (svg-path->paths p4) 0 0 'odd-even))


(define (black-queen dc)
  (draw-queen/black dc
                   black-brush
                   black-pen-1.5-round/miter
                   black-pen-1.5-butt
                   black-pen-1.5-butt/miter
                   black-pen-1.5-round
                   black-decoration-pen))

(define (white-queen dc)
  (draw-queen/white dc
                   white-brush
                   black-pen-1.5-round/miter
                   black-pen-1.5-butt
                   black-pen-1.5-butt/miter
                   black-pen-1.5-round
                   white-decoration-pen))


(define (draw-queen/black dc brush pen-round/miter pen-butt pen-butt/miter pen-round/round decoration-pen)
  (define p1 (parse-svg-path (~a "M9 26c8.5-1.5 21-1.5 27 0l2.5-12.5L31 25l-.3-14.1-5.2 "
                                 "13.6-3-14.5-3 14.5-5.2-13.6L14 25 6.5 13.5 9 26z")))
  (define p2 (parse-svg-path (~a "M9 26c0 2 1.5 2 2.5 4 1 1.5 1 1 .5 3.5-1.5 1-1.5 2.5-1.5 "
                                 "2.5-1.5 1.5.5 2.5.5 2.5 6.5 1 16.5 1 23 0 0 0 1.5-1 0-2.5 0 "
                                 "0 .5-1.5-1-2.5-.5-2.5-.5-2 .5-3.5 1-2 2.5-2 "
                                 "2.5-4-8.5-1.5-18.5-1.5-27 0z")))
  (define p3 (parse-svg-path     "M11 38.5a35 35 1 0 0 23 0"))
  (define p4 (parse-svg-path (~a "M11 29a35 35 1 0 1 23 0m-21.5 2.5h20m-21 3a35 35 1 0 0 22 0"
                                 "m-23 3a35 35 1 0 0 24 0")))

  (define (draw-circle x y r) (send dc draw-ellipse (- x r) (- y r) (* 2 r) (* 2 r)))
  ; fill
  (send dc set-brush brush)
  (send dc set-pen   pen-round/round)
  ; Note: The white quees does not have circles.
  (draw-circle  6   12 2.75)
  (draw-circle 14    9 2.75)
  (draw-circle 22.5  8 2.75)
  (draw-circle 31    9 2.75)
  (draw-circle 39   12 2.75)
  
  (send dc set-pen pen-butt)
  (draw-paths dc (svg-path->paths p1) 0 0 'odd-even)
  (draw-paths dc (svg-path->paths p2) 0 0 'odd-even)
  ; no fill
  (send dc set-brush transparent-brush)
  (send dc set-pen   pen-butt)
  (draw-paths dc (svg-path->paths p3) 0 0 'odd-even)
  (send dc set-pen   decoration-pen)
  (draw-paths dc (svg-path->paths p4) 0 0 'odd-even))

(define (draw-queen/white dc brush pen-round/miter pen-butt pen-butt/miter pen-round/round decoration-pen)
  (define p1 (parse-svg-path (~a "M8 12a2 2 0 1 1-4 0 2 2 0 1 1 4 0zm16.5-4.5a2 2 0 1 1-4 0 2 2 0 1 1 4 0z"
                                 "M41 12a2 2 0 1 1-4 0 2 2 0 1 1 4 0zM16 8.5a2 2 0 1 1-4 0 2 2 0 1 1 4 0z"
                                 "M33 9a2 2 0 1 1-4 0 2 2 0 1 1 4 0z")))
  (define p2 (parse-svg-path (~a "M9 26c8.5-1.5 21-1.5 27 0l2-12-7 11V11l-5.5 13.5-3-15-3 15-5.5-14V25L7 14l2 12z")))
  (define p3 (parse-svg-path (~a "M9 26c0 2 1.5 2 2.5 4 1 1.5 1 1 .5 3.5-1.5 1-1.5 2.5-1.5 2.5-1.5 "
                                 "1.5.5 2.5.5 2.5 6.5 1 16.5 1 23 0 0 0 1.5-1 0-2.5 0 0 "
                                 ".5-1.5-1-2.5-.5-2.5-.5-2 .5-3.5 1-2 2.5-2 2.5-4-8.5-1.5-18.5-1.5-27 0z")))
  (define p4 (parse-svg-path (~a "M11.5 30c3.5-1 18.5-1 22 0M12 33.5c6-1 15-1 21 0")))

  (define (draw-circle x y r) (send dc draw-ellipse (- x r) (- y r) (* 2 r) (* 2 r)))
  ; fill
  (send dc set-brush brush)
  (send dc set-pen   pen-round/round)  
  (send dc set-pen pen-butt)
  (draw-paths dc (svg-path->paths p1) 0 0 'odd-even)
  (draw-paths dc (svg-path->paths p2) 0 0 'odd-even)
  (send dc set-pen   pen-butt)
  (draw-paths dc (svg-path->paths p3) 0 0 'odd-even)
  ; no fill
  (send dc set-brush transparent-brush)
  (send dc set-pen   decoration-pen)
  (draw-paths dc (svg-path->paths p4) 0 0 'odd-even))


;; (draw-piece black-pawn)
;; (draw-piece white-pawn)
;; (draw-piece black-rook)
;; (draw-piece white-rook)
;; (draw-piece black-knight)
;; (draw-piece white-knight)
;; (draw-piece black-bishop)
;; (draw-piece white-bishop)
;; (draw-piece black-king)
;; (draw-piece white-king)
;; (draw-piece black-queen)
;; (draw-piece white-queen)
;; (draw-piece black-knight)
;; (draw-piece white-knight)


;; (define (draw-test)
;;   (define p     (svg-path->paths (parse-svg-path "M100,200 C100,100 250,100 250,200 S400,300 400,200")))
;;   (define bm    (make-object bitmap% 500 400))
;;   (define dc (new bitmap-dc% [bitmap bm]))
;;   (send dc set-smoothing 'smoothed)
;;   (send dc set-scale 1 1)
;;   (draw-paths dc p)
;;   bm)

;; #;(draw-test)

;; (define (draw-test2)
;;   (define p     (svg-path->paths (parse-svg-path " M 210 130 C 145 130 110 80 110 80 S 75 25 10 25 m 0 105 c 65 0 100 -50 100 -50 s 35 -55 100 -55 "
;;        #;" M 210 130 C 145 130 110 80 110 80 S 75 25 10 25 m 0 105 c 65 0 100 -50 100 -50 " )))
;;   (define bm    (make-object bitmap% 500 400))
;;   (define dc (new bitmap-dc% [bitmap bm]))
;;   (send dc set-smoothing 'smoothed)
;;   (send dc set-scale 1 1)
;;   (draw-paths dc p)
;;   bm)

;; #;(draw-test2)

;; (define (draw-arc-test)
;;   ; these paths are from the svg test suite
;;   (define ps
;;     (list #;"m 150 100 a 50 40 0 1 0 25 -70 z "
;;           #;"M 350 245 a 40 40 0 1 0 80 60"
;;           #;"M 270 30 A 50 50 0 1 0 345 30  a 50 50 0 1 0 50 0 a 50 50 0 1 0 25 0 z"
;;      #;"M 30 150 a 40 40 0 0 1 65 50 Z m 30 30 A 20 20 0 0 0 125 230 Z m 40 24  a 20 20 0 0 1 65 50 z"
;;      #;(string-append "M 215 190 A 40 200 10 0 0 265 190 A 40 200 20 0 1 315 190 "
;;                     "A 40 200 30 0 0 365 190 A 40 200 40 0 1 415 190 A  40 200 50 0 0 465 190")
;;      ; "M 215 190 A 40 200 10 0 0 265 190"
;;      #;"M100,300 l 50,-25 
;;                a25,25 -30 0,1 50,-25 l 50,-25 
;;                a25,50 -30 0,1 50,-25 l 50,-25 
;;                a25,75 -30 0,1 50,-25 l 50,-25 
;;                a25,100 -30 0,1 50,-25 l 50,-25"
     
;;      ; "M11.5 30c5.5-3 15.5-3 21 0m-21 3.5c5.5-3 15.5-3 21 0m-21 3.5c5.5-3 15.5-3 21 0"
;;      "M11.5 30c5.5-3 15.5-3 21 0 m-21 3.5"
;;      ))
              
;;   #;"M100,300 l 50,-25 
;;                a25,25 -30 0,1 50,-25 l 50,-25 
;;                a25,50 -30 0,1 50,-25 l 50,-25 
;;                a25,75 -30 0,1 50,-25 l 50,-25 
;;                a25,100 -30 0,1 50,-25 l 50,-25" 
;;   (define bm    (make-object bitmap% 800 400))
;;   (define dc (new bitmap-dc% [bitmap bm]))
;;   (send dc set-smoothing 'smoothed)
;;   (send dc set-scale 5 5)
;;   (for ([p ps])
;;     (draw-paths dc (svg-path->paths (parse-svg-path p))))
;;   bm)

;(draw-arc-test)

(define (file->index file)
  (match file
    [(regexp #rx"^[a-h]$") (- (string->number file 16) (string->number "a" 16))]
    [(? number? file) (- file 1)]
    [(? char?)        (file->index (string file))]
    [_                (error 'file->index "unexpected file: ~a" file)]))

(define (rank->index rank)
  (match rank
    [(regexp #rx"^[1-8]$") (- (string->number rank) 1)]
    [(? symbol?)           (rank->index (symbol->string rank))]
    [(? char?)             (rank->index (string rank))]
    [(? number?)           (- rank 1)]
    [_                     (error 'rank->index "unexpected rank: ~a" rank)]))

(define (file/rank s)
  (cond
    [(and (string? s) (= (string-length s) 2))
     (define sd (string-downcase s))
     (match sd
       [(regexp #rx"^[a-h][1-8]$")
        (define file (string-ref sd 0))
        (define rank (string-ref sd 1))
        (list (+ (file->index file) 1)
              (+ (rank->index rank) 1))])]
    [(symbol? s)
     (file/rank (symbol->string s))]
    [else
     (error 'file/rank "unexpected file/rank, got: ~a" s)]))

(define (file/rank->index s)
  (match (file/rank s)
    [(list f r) (list (file->index f)
                      (rank->index r))]))

(define (piece->pict piece [w 45] [h 45])
  (define who 'piece->pict)
  (define (blank-piece dc) (void))
  (define draw-ht (hash "wp" white-pawn
                        "wr" white-rook
                        "wb" white-bishop
                        "wn" white-knight
                        "wk" white-king
                        "wq" white-queen
                        "bp" black-pawn
                        "br" black-rook
                        "bb" black-bishop
                        "bn" black-knight
                        "bk" black-king
                        "bq" black-queen
                        "  " blank-piece
                        
                        "p" white-pawn
                        "r" white-rook
                        "b" white-bishop
                        "n" white-knight
                        "k" white-king
                        "q" white-queen
                        "P" black-pawn
                        "R" black-rook
                        "B" black-bishop
                        "N" black-knight
                        "K" black-king
                        "Q" black-queen

                        " "  blank-piece
                        "_"  blank-piece))
  (define (draw->pict draw)
    (make-chess-pict draw w h))

  (cond
    [(and (string? piece) (= (string-length piece) 1))
     (match piece
       [(regexp #rx"^[prnbqkPRNBQK ]$")
        (draw->pict (hash-ref draw-ht piece #f))]
       [_ (error who "unexpected piece 1, got: ~a" piece)])]
    [(and (string? piece) (= (string-length piece) 2))
     (match (string-downcase piece)
       [(regexp #rx"^[wb][prnbqk]$")
        (draw->pict (hash-ref draw-ht (string-downcase piece)))]
       [_ (error who "unexpected piece 2, got: ~a" piece)])]
    [(symbol? piece)
     (piece->pict (symbol->string piece) w h)]
    [(char? piece)
     (piece->pict (string piece) w h)]
    [else
     (write piece)
     (error who "unexpected piece 3, got: ~a" piece)]))


(define (board-pict ranks [w 45] [h 45]
                    #:to   [moved-to   #f]
                    #:from [moved-from #f])
  (define (background-brush file rank)
    (define x (file->index file))
    (define y (rank->index rank))
    (if (even? (+ x y)) dark-brush light-brush))
  (define (rank->pict rank r)
    (apply pict:hc-append
           (for/list ([p rank] [f (in-range 1 9)])
             (define piece (piece->pict p w h))
             (define tile-brush
               (if (or (and moved-to   (equal? (file/rank moved-to)   (list f r)))
                       (and moved-from (equal? (file/rank moved-from) (list f r))))
                   moved-brush
                   (background-brush f r)))
             (define tile (filled-rectangle w h #:draw-border? #f #:brush tile-brush))
             (pict:cc-superimpose tile piece))))
  (define (ranks->pict ranks)
    (apply pict:vc-append
           (for/list ([r    (in-range 8 0 -1)]
                      [rank ranks])
             (rank->pict rank r))))

  (ranks->pict ranks))

(define (test-board #:from [moved-from #f]
                    #:to   [moved-to #f])
  (displayln (file/rank moved-to))
  (displayln (file/rank moved-from))
  (board-pict
   '("RNBQKBNR"
     "PPPPPPPP"
     "        "
     "        "
     "    p   "
     "        "
     "pppp ppp"
     "rnbqkbnr")
   45 45
   #:from moved-from
   #:to   moved-to
   ))
    


;; (pict:hc-append (make-chess-pict black-bishop 145 145)
;;                 (make-chess-pict black-bishop 45 45))

;; (pict:vc-append (pict:hc-append (wP) (wP) (wP) (wP) (wP) (wP) (wP) (wP))
;;                 (pict:hc-append (wR) (wN) (wB) (wQ) (wK) (wB) (wN) (wR)))

;; (test-board #:from "e2" #:to "e4")
