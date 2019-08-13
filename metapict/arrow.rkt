#lang racket/base
;;; Arrows
(provide 
 arrow-head          ; curve in the shape of an arrow head
 harpoon-up          ; curve in the shape of an up harpoon
 harpoon-down        ; curve in the shape of an down harpoon
 line-head           ; curve in the shape of a line
 reverse-head        ; given head maker, return new head maker that reverses given head
 draw-arrow          ; draw curve then draw arrow head at the end
 draw-double-arrow   ; draw curve then draw arrow heads at the start and end
 ;; parameters
 ahlength            ; arrow head length
 ahangle             ; arrow head angle
 ahflankangle        ; arrow flank angle
 ahtailcurvature     ; arrow tail curvature
 ahratio)            ; arrow ratio

(require "angles.rkt" "def.rkt" "device.rkt" "curve.rkt" "trans.rkt" "shapes.rkt" "draw.rkt"
         "path.rkt" "trig.rkt" "pt-vec.rkt" "structs.rkt" "angles.rkt" "color.rkt"
         "parameters.rkt" "pict.rkt")

; The shape is inspired by:
;     http://www.ntg.nl/maps/36/19.pdf

(def ahlength        (make-parameter (px 4)))   ; default arrowhead length 4bp
(def ahangle         (make-parameter 45))       ; default head angle 45 degrees
(def ahflankangle    (make-parameter 10))       ; default "curvature" of flank (in degrees)
(def ahtailcurvature (make-parameter 2))        ; default "curvature" of the back  todo!
(def ahratio         (make-parameter 0.9))      ; default "curvature" of the back  todo!

(struct head/info (head tipx length  length-ratio))

(define (arrow-head #:length            [l #f] 
                    #:length-ratio      [r #f] 
                    #:head-angle        [α #f]  ; angle in degrees
                    #:flank-indentation [β #f]  ; angle in degrees  (todo: better word?)
                    #:tail-indentation  [γ #f]) ; angle in degrees 
  ; The attachment point of this arrow is (pt 0 0).
  ; The arrow points in the direction (vec 1 0).
  (unless l (set! l (ahlength)))
  (unless r (set! r (ahratio)))
  (unless α (set! α (ahangle)))
  (unless β (set! β (ahflankangle)))
  (unless γ (set! γ (ahtailcurvature)))
  ; (set! l (norm (pt- (devpt (values l l)) origo))) ; xxx
  ; See http://www.ntg.nl/maps/36/19.pdf
  (def xmax (* r l))
  (def xmin (- xmax l))
  (def α/2 (/ α 2))
  (def -α/2 (- α/2))
  (def ymax (* l (tan (rad α/2))))
  (def ymin (- ymax))
  (def tipx xmax)
  (def tip (pt tipx 0))
  (def A (pt 0 0))       ; tail middle
  (def B (pt xmin ymax)) ; tail top
  (def C (pt xmax 0))    ; tip
  (def D (pt xmin ymin)) ; tail bottom
  (def BC  (curve B (dir (- -α/2 β))     .. (dir (+ -α/2 β)) C))
  (def CD  (curve C (dir (-  α/2 β 180)) .. (dir (+ -180 α/2  β)) D))
  (def DAB (curve D .. A .. B))
  (def head (curve-append (curve-append DAB BC) CD))
  (head/info head tipx l r))

(define (harpoon-up #:length            [l #f] 
                    #:length-ratio      [r #f] 
                    #:head-angle        [α #f]  ; angle in degrees
                    #:flank-indentation [β #f]  ; angle in degrees  (todo: better word?)
                    #:tail-indentation  [γ #f]) ; angle in degrees 
  ; The "attachment point" of this arrow is (pt 0 0).
  ; The arrow points in the direction (vec 1 0).
  (unless l (set! l (ahlength)))
  (unless r (set! r (ahratio)))
  (unless α (set! α (ahangle)))
  (unless β (set! β (ahflankangle)))
  (unless γ (set! γ (ahtailcurvature)))
  ; (set! l (norm (pt- (devpt (values l l)) origo))) ; xxx
  ; See http://www.ntg.nl/maps/36/19.pdf
  (def xmax (* r l))
  (def xmin (- xmax l))
  (def α/2 (/ α 2))
  (def -α/2 (- α/2))
  (def ymax (* l (tan (rad α/2))))
  (def ymin (- ymax))
  (def tip (pt xmax 0))
  (def A (pt 0 0))       ; tail middle
  (def B (pt xmin ymax)) ; tail top
  (def C (pt xmax 0))    ; tip
  (def D (pt xmin ymin)) ; tail bottom
  (def BC  (curve B (dir (- -α/2 β))     .. (dir (+ -α/2 β)) C))
  (def CD  (curve C (dir (-  α/2 β 180)) .. (dir (+ -180 α/2  β)) D))
  (def AB (subcurve (curve D .. A .. B) 1 2))
  (def CA  (curve C -- A))
  (def head (curve-append (curve-append AB BC) CA))
  (head/info head xmax l r))

(define (harpoon-down #:length            [l #f] 
                      #:length-ratio      [r #f] 
                      #:head-angle        [α #f]  ; angle in degrees
                      #:flank-indentation [β #f]  ; angle in degrees  (todo: better word?)
                      #:tail-indentation  [γ #f]) ; angle in degrees 
  (defm (head/info head tipx l r)
    (harpoon-up #:length            l
                     #:length-ratio      r 
                     #:head-angle        α
                     #:flank-indentation β
                     #:tail-indentation  γ))
  (head/info (flipy head) tipx l r))

; return the curve for the arrow head placed at the end of the curve c
(define (place-arrow-head c ah)
  (defm (head/info head tipx l r) ah)
  (unless l (set! l (ahlength)))
  (unless r (set! r (ahratio)))
  (defm (and end (pt endx endy)) (end-point c))
  (def d (direction-of c (curve-length c)))
  ((shifted endx endy)
   (rotated (if (equal? d (vec 0 0)) 0 (angle d)))
   (shifted (- tipx) 0)
   head))

(define (reverse-head head-maker
                      #:length            [l #f] 
                      #:length-ratio      [r #f] 
                      #:head-angle        [α #f]  ; angle in degrees
                      #:flank-indentation [β #f]  ; angle in degrees  (todo: better word?)
                      #:tail-indentation  [γ #f])
  (define (rev #:length            [l #f] 
               #:length-ratio      [r #f] 
               #:head-angle        [α #f]  ; angle in degrees
               #:flank-indentation [β #f]  ; angle in degrees  (todo: better word?)
               #:tail-indentation  [γ #f])
    (unless l (set! l (ahlength)))
    (shifted l 0 (flipx (head-maker #:length            l
                                    #:length-ratio      r
                                    #:head-angle        α
                                    #:flank-indentation β
                                    #:tail-indentation  γ))))
  rev)

(define (line-head  #:length            [l #f]
                    #:length-ratio      [r #f] 
                    #:head-angle        [α #f]  ; angle in degrees
                    #:flank-indentation [β #f]  ; angle in degrees (todo: better word?)
                    #:tail-indentation  [γ #f]) ; angle in degrees
  ; The "attachment point" of this "arrow" is (pt 0 0).
  ; The arrow points in the direction (vec 1 0).
  (unless l (set! l (ahlength)))
  (unless r (set! r (ahratio)))
  (unless α (set! α (ahangle)))
  ; See http://www.ntg.nl/maps/36/19.pdf
  (def xmax (* r l))
  (def  α/2 (/ α 2))
  (def -α/2 (- α/2))
  (def ymax (* l (tan (rad α/2))))
  (def ymin (- ymax))
  (def B (pt 0 ymax)) ; top
  (def D (pt 0 ymin)) ; bottom
  (def head (curve B -- D))
  (head/info head 0 l r))

(def (draw-arrow c 
                 #:length            [l #f] 
                 #:length-ratio      [r #f] 
                 #:head-angle        [α #f]  ; angle in degrees
                 #:flank-indentation [β #f]  ; angle in degrees  (todo: better word?)
                 #:tail-indentation  [γ #f]
                 #:fill-head         [fill-head? #t]
                 #:fill-tail         [fill-tail? #t]
                 #:head              [head arrow-head]
                 #:tail              [tail #f]
                 ; colors
                 ; The general #:colors sets both stem and head color (but doesn't override)
                 #:color              [col (current-arrow-color)]
                 ; The specific colors
                 #:stem-color         [stem-col         (current-arrow-stem-color)]
                 #:head-color         [head-col         (current-arrow-head-color)]
                 #:head-outline-color [head-outline-col (current-arrow-head-outline-color)]
                 ; hooks for custom draw functions
                 #:draw-head         [draw-head    (or (current-draw-arrow-head) filldraw)]
                 #:draw-stem         [draw-stem    (or (current-draw-arrow-stem) draw)]
                 #:draw-outline      [draw-outline (or (current-draw-arrow-head-outline) draw)])
  (def the-head (if head
                    (place-arrow-head 
                     c (head
                        #:length            l
                        #:length-ratio      r
                        #:head-angle        α
                        #:flank-indentation β
                        #:tail-indentation  γ))
                    empty-curve))
  (def the-tail (if tail
                    (place-arrow-head 
                     (curve-reverse c)
                     (tail
                     #:length            l
                     #:length-ratio      r
                     #:head-angle        α
                     #:flank-indentation β
                     #:tail-indentation  γ))
                    empty-curve))

  (define (make-pen-wrap c)   (λ (t) (if c (pencolor c (t)) (t))))
  (define (make-brush-wrap c) (λ (t) (if c (brushcolor c (t)) (t))))
  (def stem-wrap    (make-pen-wrap   (or stem-col col)))
  (def outline-wrap (make-pen-wrap   (or head-outline-col head-col stem-col col)))
  (def fill-wrap    (make-brush-wrap (or head-col stem-col col)))
  (define (head-wrap t) (outline-wrap (λ () (fill-wrap t))))
  
  (stem-wrap
   (λ ()
     (draw-stem c 
                (if fill-head?
                    (head-wrap    (λ () (draw-head the-head head-col head-outline-col)))
                    (outline-wrap (λ () (draw-outline the-head))))
                (if fill-tail?
                    (head-wrap    (λ () (draw-head the-tail head-col head-outline-col)))
                    (outline-wrap (λ () (draw-outline the-tail))))))))

(def (draw-double-arrow c 
                        #:length            [l #f] 
                        #:length-ratio      [r #f] 
                        #:head-angle        [α #f]  ; angle in degrees
                        #:flank-indentation [β #f]  ; angle in degrees  (todo: better word?)
                        #:tail-indentation  [γ #f]

                        #:color              [col (current-arrow-color)]
                        ; The specific colors
                        #:stem-color         [stem-col         (current-arrow-stem-color)]
                        #:head-color         [head-col         (current-arrow-head-color)]
                        #:head-outline-color [head-outline-col (current-arrow-head-outline-color)]
                        ; hooks for custom draw functions
                        #:draw-head         [draw-head    (or (current-draw-arrow-head) filldraw)]
                        #:draw-stem         [draw-stem    (or (current-draw-arrow-stem) draw)]
                        #:draw-outline      [draw-outline (or (current-draw-arrow-head-outline) draw)])

  (draw (draw-arrow c 
                    #:length             l
                    #:length-ratio       r
                    #:head-angle         α
                    #:flank-indentation  β
                    #:tail-indentation   γ
                    #:color              col
                    #:stem-color         stem-col
                    #:head-color         head-col
                    #:head-outline-color head-outline-col
                    #:draw-head         [draw-head    (or (current-draw-arrow-head) filldraw)]
                    #:draw-stem         [draw-stem    (or (current-draw-arrow-stem) draw)]
                    #:draw-outline      [draw-outline (or (current-draw-arrow-head-outline) draw)])
        (draw-arrow (curve-reverse c) 
                    #:length            l
                    #:length-ratio      r
                    #:head-angle        α
                    #:flank-indentation β
                    #:tail-indentation  γ
                    #:color             col
                    #:stem-color         stem-col
                    #:head-color         head-col
                    #:head-outline-color head-outline-col
                    #:draw-head         [draw-head    (or (current-draw-arrow-head) filldraw)]
                    #:draw-stem         [draw-stem    (or (current-draw-arrow-stem) draw)]
                    #:draw-outline      [draw-outline (or (current-draw-arrow-head-outline) draw)])))

(def (arrow-head-mp c) ; plain
  ; Old MetaPost Style - not too pretty
  (def tip (point-of c (curve-length c)))
  (def stem (cut-before c ((scaled (ahlength)) unitcircle)))
  (def side1 ((rotated    (ahangle))  stem))
  (def side2 ((rotated (- (ahangle))) stem))
  (draw side1 side2 (curve (point-of side1 0) -- (point-of side2 0))))
