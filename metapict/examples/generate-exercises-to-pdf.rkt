#lang racket
(require metapict racket/draw math/base)

; A study card has two sides: a front and a back.
; Both sides are represented as picts.
(struct card (front back))

(def a4-width  595) ; in pt
(def a4-height 842) ; in pt

(define (use-a4-paper)
  (define setup (new ps-setup%))
  (current-ps-setup setup)
  (send setup set-paper-name "A4 210 x 297 mm")
  ; introduce margins such that the result is centered
  (def α 0.9)
  (send setup set-margin (/ (* (- 1 α) a4-width) 2) (/ (* (- 1 α) a4-height) 2))
  (send setup set-editor-margin 0 0)
  (send setup set-scaling α α))

(define (new-pdf-dc filename)
  (use-a4-paper)
  (send (current-ps-setup) set-file filename)
  (new pdf-dc% [interactive #f] [parent #f] [use-paper-bbox #t] [as-eps #f] [output #f]))

(define (start dc)
  (send dc start-doc "Printing exercises")
  (send dc start-page))

(define (end dc)
  (send dc end-page)
  (send dc end-doc))

(define (new-page dc)
  (send dc end-page)
  (send dc start-page))


(def card-rows 5)
(def card-cols 2)
(def card-width  (/ a4-width  card-cols))
(def card-height (/ a4-height card-rows))

(define (new-exercises)
  (def dc (new-pdf-dc "exercises.pdf"))
  (start dc)
  (exercise1 dc)
  (for ([i 5])
    (new-page dc)
    (exercise1 dc))
  (end dc))

(define (draw-faces dc fs rowf)
  (define (framed p) (frame p #:color "gray" #:segment 5))
  (def rows (reverse 
             (let loop ([xs (map framed fs)])
               (match xs
                 [(list* x y more) (cons (list x y) (loop more))]
                 [_                xs]))))
  (draw-pict (apply vc-append (for/list ([row (map rowf rows)]) (apply hc-append row))) dc 0 0))

(define (exercise1 dc)
  (def cards (for/list ([i 10]) (exercise-card:power-simple
                                 #;exercise-card:solve-linear-equals-linear
                                 #;exercise-card:solve-linear-equals-constant
                                 #;exercise-card:random)))
  (draw-faces dc (map card-front cards) values)
  (new-page dc)
  (draw-faces dc (map card-back cards) reverse))

; face : pict -> pict
;     place the pict p in the middle of the face of a card
(define (face p)
  (define (amount total partial) (/ (- total partial) 2)) 
  (inset p (amount card-width (pict-width p)) (amount card-height (pict-height p))))

(define (~point x y)
  (~a "(" x "," y ")"))

(define (exercise-card:two-points-to-slope)
  (def (X) (random-integer -5 6))
  (defv (x1 x2 y1 y2) (values (X) (X) (X) (X)))
  (cond 
    [(= x1 x2) (exercise-card:two-points-to-slope)]
    [else (def a (/ (- y2 y1) (- x2 x1)))
          (def b (- y1 (* a x1)))
          (card
           (face 
            (vl-append 
             (text "En linje med ligning y=ax+b")
             (text (~a "går gennem " (~point x1 y1) " og " (~point x2 y2) "."))
             (text "")
             (text (~a "Hvad er linjens hældning, a?"))))
           (face 
            (vl-append (text (~a "a = " a)))))]))

(define (exercise-card:point-and-slope-to-intersection)
  (def (X) (random-integer -5 6))
  (defv (x1 y1 a) (values (X) (X) (X)))
  (def b (- y1 (* a x1)))
  (card
   (face 
    (vl-append 
     (text "En linje med ligning y=ax+b går")
     (text (~a "gennem " (~point x1 y1) " og har hældningen a = " a "."))
     (text "")
     (text (~a "Hvad er linjens konstantled, b?"))))
   (face 
    (vl-append (text (~a "b = " b))))))

(define (line-on-grid win a b)
  (defm (window xmin xmax ymin ymax) win)
  (def boundary 
    (scaled 1.01 ; avoids problem with the line y=ymax which otherwise is clipped
            (curve (pt xmin ymin) -- (pt xmax ymin) -- (pt xmax ymax) -- (pt xmin ymax) -- cycle)))
  (define (grid)
    (define (hline y) (curve (pt xmin y) -- (pt xmax y)))
    (define (vline x) (curve (pt x ymin) -- (pt x ymax)))
    (draw* (append (for/list ([x (in-range xmin (+ xmax 1))]) (vline x))
                   (for/list ([y (in-range ymin (+ ymax 1))]) (hline y)))))
  (define (ticks)
    (draw (curve (pt 1 -1/3) -- (pt 1 1/3))
          (curve (pt -1/3 1) -- (pt 1/3 1))))
  (define (axes)
    (def x-axis (curve (pt xmin 0) -- (pt xmax 0)))
    (def y-axis (curve (pt 0 ymin) -- (pt 0 ymax)))
    (ahlength 1/2)
    (draw (draw-arrow x-axis)
          (draw-arrow y-axis)
          (label-rt  "x" (pt xmax 0))
          (label-top "y" (pt 0 ymax))))
  (define (f x) (+ (* a x) b))
  (with-window win
    (draw (color "gray" (grid))
          (axes) (ticks)
          (color "red" (clipped (draw (curve (pt xmin (f xmin)) -- (pt xmax (f xmax)))) boundary)))))

(define (~linear a b)
  ; return ax+b as string
  (define (one? x)    (= x 1))
  (define (negone? x) (= x -1))
  (define (~b b)
    (match b
      [(? zero?)     ""]
      [(? negative?) (~a b)]
      [_             (~a "+" b)]))
  (def +b (~b b))
  (match a
    [(? one?)      (~a   "x" +b)]
    [(? negone?)   (~a  "-x" +b)]
    [(? zero?)               +b]
    [_             (~a a "x" +b)]))

(define (~equation a b)
  (~a " y = " (~linear a b)))

(define (exercise-card:line-to-equation)
  (def (X) (random-integer -5 6))
  (defv (a b) (values (X) (X)))
  (def win (window -5 5 -5 5))
  (card
   (face 
    (vc-append 
     (line-on-grid win a b)
     (text "Linjens ligning?")))
   (face 
    (vl-append (text (~equation a b))))))

(define (exercise-card:solve-linear-equals-constant)
  (def (X) (random-integer -5 6))
  (defv (a b x) (values (X) (X) (X)))
  (cond
    [(or (= a 0) (= b 0))
     (exercise-card:solve-linear-equals-constant)]
    [else
     (def y (+ (* a x) b))
     (card
      (face 
       (vc-append 
        (text "Løs ligningen:\n")
        (text (~a (~linear a b) " = " y))))
      (face 
       (vl-append (text (~a " x = " x)))))]))

(define (exercise-card:solve-linear-equals-linear)
  (def (X) (random-integer -5 6))
  (defv (a b c d x) (values (X) (X) (X) (X) (X)))
  (def y1 (+ (* a x) b))
  (def y2 (+ (* c x) d))
  (cond
    [(or (= a 0) (= b 0) (= c 0) (= d 0) (not (= y1 y2)) (and (= a c) (= b d)))
     (exercise-card:solve-linear-equals-linear)]
    [else
     (card
      (face 
       (vc-append 
        (text "Løs ligningen:\n")
        (text (~a (~linear a b) " = " (~linear c d)))))
      (face 
       (vl-append (text (~a " x = " x)))))]))

(define (exercise-card:power-simple)
  ; TODO : Finish
  (def (X) (random-integer -5 6))
  (defv (a b) (values (X) (X)))
  (cond 
    [(and (<= a 0) (< b 0))
     (exercise-card:power-simple)]
    [else
     (card
      (face 
       (vc-append 
        (text "Udregn:\n")
        (pict-expt (text (~neg a)) (text (~a b)))))
      (face 
       (vl-append (text (~a (expt a b))))))]))

(define (~neg x) ; use parens if negative
  (match x [(? negative?) (~a "(" x ")")] [_ (~a x)]))

(define (pict-expt a b)
  ; display a ^ b
  (table 2 (list (text "") b a (text ""))
         (cons cb-superimpose cb-superimpose) 
         (cons cc-superimpose cc-superimpose)
         (cons 0 0) (cons 0 0)))

(define (exercise-card:random)
  (def exercises (list exercise-card:line-to-equation
                       exercise-card:point-and-slope-to-intersection
                       exercise-card:two-points-to-slope
                       ; exercise-card:solve-linear-equals-constant
                       ; exercise-card:solve-linear-equals-linear
                       ))
  ((list-ref exercises (random (length exercises)))))

(define (exercise1-card)
  (def (a) (random-integer -5 6))
  (def (b) (random-integer -5 6))
  (frame (card (text (~equation (a) (b))))))

(new-exercises)


