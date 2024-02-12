#lang racket/base

;;;
;;; Arrows and arrow heads
;;;

; This module provides two functions to draw arrows.
; To draw an arrow, use the function draw-arrow which takes
; a curve and an arrow head as input and draws the curve placing
; the arrow head at the end of the curve. The tip of the arrow
; will be placed at the end point of the curve.

; There are a lot of bells and whistles that can be adjusted, but the
; above describes the standard way of using draw-arrow.

; To draw an arrow with arrow heads in both ends, use draw-double-arrow.

; Note: This module started as an implementation of the arrow heads
;       in Hans van der Meer's paper: "Blocs and Arrows with MetaPost".
;       Feature creep has set in, so a reorganization of the code
;       would make it easier for users to implement their own arrows.
;       The Right Thing would be to represent arrows with structures,
;       that has the property prop:drawable.


;;; Arrows
(provide 
 draw-arrow          ; draw curve then draw arrow head at the end
 draw-double-arrow   ; draw curve then draw arrow heads at the start and end

 arrow-head          ; curve in the shape of an arrow head (for filling)
 arrow-head/no-fill  ; same, but unfilled version (omits backend)
 
 harpoon-up          ; curve in the shape of an up harpoon
 harpoon-down        ; curve in the shape of an down harpoon
 hook-head reverse-hook-head
 line-head           ; curve in the shape of a line
 reverse-head        ; given head maker, return new head maker that reverses given head

 ;; parameters
 ahlength            ; arrow head length
 ahangle             ; arrow head angle
 ahflankangle        ; arrow flank angle
 ahtailcurvature     ; arrow tail curvature
 ahratio)            ; arrow ratio

(require "angles.rkt" "def.rkt" "device.rkt" "curve.rkt" "trans.rkt" "shapes.rkt" "draw.rkt"
         "path.rkt" "trig.rkt" "pt-vec.rkt" "structs.rkt" "angles.rkt" "color.rkt"
         "parameters.rkt" "pict.rkt" "path-operations.rkt")

; The shape is inspired by:
;     http://www.ntg.nl/maps/36/19.pdf

(def ahlength        (make-parameter (px 4)))   ; default arrowhead length 4bp
(def ahangle         (make-parameter 45))       ; default head angle 45 degrees
(def ahflankangle    (make-parameter 10))       ; default "curvature" of flank (in degrees)
(def ahtailcurvature (make-parameter 2))        ; default "curvature" of the back  todo!
(def ahratio         (make-parameter 0.9))      ; default "curvature" of the back  todo!

(struct head/info: (head tipx fill? length length-ratio))

(define (head/info head tipx length length-ratio #:fill? [fill? #t])
  (head/info: head tipx fill? length length-ratio ))

(define (arrow-head/no-fill #:length            [l #f] 
                            #:length-ratio      [r #f] 
                            #:head-angle        [α #f]  ; angle in degrees
                            #:flank-indentation [β #f]  ; angle in degrees  (todo: better word?)
                            #:tail-indentation  [γ #f]) ; angle in degrees
  (arrow-head #:length            l  
              #:length-ratio      r  
              #:head-angle        α 
              #:flank-indentation β 
              #:tail-indentation  γ 
              #:no-backend?       #t
              #:fill              #f))

  
(define (arrow-head #:length            [l #f] 
                    #:length-ratio      [r #f] 
                    #:head-angle        [α #f]  ; angle in degrees
                    #:flank-indentation [β #f]  ; angle in degrees  (todo: better word?)
                    #:tail-indentation  [γ #f]   ; angle in degrees
                    #:no-backend?       [no-backend? #f]
                    #:fill              [fill? #t])  ; used with unfilled head
  ; The attachment point of this arrow is (pt 0 0).
  ; The arrow points in the direction (vec 1 0).
  (unless l (set! l (ahlength)))
  (unless r (set! r (ahratio)))
  (unless α (set! α (ahangle)))
  (unless β (set! β (ahflankangle)))
  (unless γ (set! γ (ahtailcurvature)))
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
  (def DA-angle (let-values ([(_ θ) (@ (pt- A D))]) (deg θ))) ; angle from x-axis to vector DA
  (def AB-angle (let-values ([(_ θ) (@ (pt- B A))]) (deg θ))) ; angle from x-axis to vector AB
  (def DAB (curve D (dir (- DA-angle γ)) .. (dir (+ DA-angle γ)) A
                    (dir (- AB-angle γ)) .. (dir (+ AB-angle γ)) B))
  (def head (if no-backend?
                (curve-append BC CD) ; omit back part
                (curve-append (curve-append DAB BC) CD)))
  (head/info head tipx l r #:fill? fill?))



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
  (defm (head/info: head tipx _ L R)  ; (head/info: head tipx fill? length length-ratio)
    (harpoon-up #:length            l
                #:length-ratio      r 
                #:head-angle        α
                #:flank-indentation β
                #:tail-indentation  γ))
  (head/info (flipy head) tipx L R #:fill? #t))

; return the curve for the arrow head placed at the end of the curve c
(define (attach-arrow-head c ah)
  (def d (direction-of c (curve-length c)))
  (def e (end-point c))
  (place-arrow-head e d ah))

(define (attach-arrow-heads c ah n dist)
  ; place n heads with a distance of dist (logical distance)
  (define len (arc-length c))
  (for/list ([i n])
    (def t (arc-time c (- len (* i dist))))
    (def p (point-of c t))
    (def d (direction-of c t))
    (place-arrow-head p d ah)))

; return curve for arrow head ah with tip at p in the direction d
(define (place-arrow-head p d ah)
  (defm (and end (pt endx endy)) p)
  (defm (head/info: head tipx fill? l r) ah)
  (unless l (set! l (ahlength)))
  (unless r (set! r (ahratio)))
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
    (define h (head-maker #:length            l 
                          #:length-ratio      r 
                          #:head-angle        α
                          #:flank-indentation β
                          #:tail-indentation  γ))
    (defm (head/info: head tipx fill? L R) h)  ; (head/info: head tipx fill? length length-ratio)
    (head/info (shifted l 0 (flipx head)) tipx L R #:fill? #t))
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


; The hook arrow is a little half circle.
; Used as tail it signals an injective function.
;   TeX:  \hookrightarrow
(define (hook-head  #:length            [l #f]
                    #:length-ratio      [r #f]
                    ; the following are ignored
                    #:head-angle        [α #f]  ; angle in degrees
                    #:flank-indentation [β #f]  ; angle in degrees  (todo: better word?)
                    #:tail-indentation  [γ #f])
  ; The "attachment point" of this "arrow" is (pt 0 0).
  ; The arrow points in the direction (vec 1 0).
  (unless l (set! l (ahlength)))
  (unless r (set! r (ahratio)))  
  (unless α (set! α (ahangle)))
  ; See http://www.ntg.nl/maps/36/19.pdf
  (def  α/2 (/ α 2))
  (def ymax (* l (tan (rad α/2))))
  (def radius (* 0.5 ymax))
  (def head (flipy (curve (pt 0 0) -arc radius 270 90)))
  (head/info head 0 l r #:fill? #f))

(define (reverse-hook-head  #:length            [l #f]
                            #:length-ratio      [r #f]
                            ; the following are ignored
                            #:head-angle        [α #f]  ; angle in degrees
                            #:flank-indentation [β #f]  ; angle in degrees  (todo: better word?)
                            #:tail-indentation  [γ #f])
  ; The "attachment point" of this "arrow" is (pt 0 0).
  ; The arrow points in the direction (vec 1 0).
  (unless l (set! l (ahlength)))
  (unless r (set! r (ahratio)))  
  (unless α (set! α (ahangle)))
  ; See http://www.ntg.nl/maps/36/19.pdf
  (def  α/2 (/ α 2))
  (def ymax (* l (tan (rad α/2))))
  (def radius (* 0.5 ymax))
  (def head (curve (pt 0 0) -arc radius 270 90))
  (head/info head 0 l r #:fill? #f))

(def (draw-arrow c 
                 #:length            [l #f] 
                 #:length-ratio      [r #f] 
                 #:head-angle        [α #f]  ; angle in degrees
                 #:flank-indentation [β #f]  ; angle in degrees  (todo: better word?)
                 #:tail-indentation  [γ #f]
                 #:fill-head         [fill-head? #t]
                 #:fill-tail         [fill-tail? #t]
                 #:head              [head (or (current-arrow-head) arrow-head)]
                 #:tail              [tail (or (current-arrow-tail) #f)]
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
                 #:draw-outline      [draw-outline (or (current-draw-arrow-head-outline) draw)]
                 #:head-count        [n 1]           ; number of heads
                 #:tail-count        [m 1]           ; number of tails
                 #:gap-ratio         [gap-ratio .5]   ; gap ration for distance between tips
                 )
  (def head/info (and head (head #:length            l
                                 #:length-ratio      r
                                 #:head-angle        α
                                 #:flank-indentation β
                                 #:tail-indentation  γ)))
  (def tail/info (and tail (tail #:length            l
                                 #:length-ratio      r
                                 #:head-angle        α
                                 #:flank-indentation β
                                 #:tail-indentation  γ)))
  (def dist (* gap-ratio (ahlength)))
  (def the-head (if head (attach-arrow-heads  c                 head/info n dist) (list empty-curve)))
  (def the-tail (if tail (attach-arrow-heads  (curve-reverse c) tail/info m dist) (list empty-curve)))

  (define (make-pen-wrap c)   (λ (t) (if c (pencolor c (t)) (t))))
  (define (make-brush-wrap c) (λ (t) (if c (brushcolor c (t)) (t))))
  (def stem-wrap    (make-pen-wrap   (or stem-col col)))
  (def outline-wrap (make-pen-wrap   (or head-outline-col head-col stem-col col)))
  (def fill-wrap    (make-brush-wrap (or head-col stem-col col)))

  (define (head-wrap t) (outline-wrap (λ () (fill-wrap t))))
  
  (stem-wrap
   (λ ()
     (draw-stem c 
                (if (and fill-head? head (head/info:-fill? head/info))
                    (head-wrap    (λ () (for/draw ([a-head the-head])
                                                  (draw-head a-head head-col head-outline-col))))
                    (outline-wrap (λ () (for/draw ([a-head the-head])
                                                  (draw-outline a-head)))))
                (if (and fill-tail? tail (head/info:-fill? tail/info))
                    (head-wrap    (λ () (for/draw ([a-tail the-tail])
                                                  (draw-head a-tail head-col head-outline-col))))
                    (outline-wrap (λ () (for/draw ([a-tail the-tail])
                                                  (draw-outline a-tail)))))))))

(define (draw-double-arrow c 
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
                    #:draw-head         draw-head
                    #:draw-stem         draw-stem
                    #:draw-outline      draw-outline)
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
                    #:draw-head         draw-head
                    #:draw-stem         draw-stem
                    #:draw-outline      draw-outline)))

(def (arrow-head-mp c) ; plain
  ; Old MetaPost Style - not too pretty
  (def tip (point-of c (curve-length c)))
  (def stem (cut-before c ((scaled (ahlength)) unitcircle)))
  (def side1 ((rotated    (ahangle))  stem))
  (def side2 ((rotated (- (ahangle))) stem))
  (draw side1 side2 (curve (point-of side1 0) -- (point-of side2 0))))
