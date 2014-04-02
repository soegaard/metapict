#lang racket/base
;;; Arrows
(provide 
 arrow-head          ; curve in the shape of an arrow head
 harpoon-up          ; curve in the shape of an up harpoon
 harpoon-down        ; curve in the shape of an up harpoon 
 draw-arrow          ; draw curve then draw arrow head at the end
 draw-double-arrow   ; draw curve then draw arrow heads at the start and end
 ;; parameters
 ahlength            ; arrow head length
 ahangle             ; arrow head angle
 ahflankangle        ; arrow flank angle
 ahtailcurvature     ; arrow tail curvature
 ahratio)            ; arrow ratio

(require "angles.rkt" "def.rkt" "device.rkt" "curve.rkt" "trans.rkt" "shapes.rkt" "draw.rkt"
         "path.rkt" "trig.rkt" "pt-vec.rkt" "structs.rkt" "angles.rkt")

; The shape is inspired by:
;     http://www.ntg.nl/maps/36/19.pdf

(def ahlength        (make-parameter (px 4)))   ; default arrowhead length 4bp
(def ahangle         (make-parameter 45))       ; default head angle 45 degrees
(def ahflankangle    (make-parameter 10))       ; default "curvature" of flank (in degrees)
(def ahtailcurvature (make-parameter 2))        ; default "curvature" of the back  todo!
(def ahratio         (make-parameter 0.9))      ; default "curvature" of the back  todo!


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
  (def tip (pt xmax 0))
  (def A (pt 0 0))       ; tail middle
  (def B (pt xmin ymax)) ; tail top
  (def C (pt xmax 0))    ; tip
  (def D (pt xmin ymin)) ; tail bottom
  (def BC  (curve B (dir (- -α/2 β))     .. (dir (+ -α/2 β)) C))
  (def CD  (curve C (dir (-  α/2 β 180)) .. (dir (+ -180 α/2  β)) D))
  (def DAB (curve D .. A .. B))
  (curve-append (curve-append DAB BC) CD))

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
  (curve-append (curve-append AB BC) CA))

(define (harpoon-down #:length            [l #f] 
                      #:length-ratio      [r #f] 
                      #:head-angle        [α #f]  ; angle in degrees
                      #:flank-indentation [β #f]  ; angle in degrees  (todo: better word?)
                      #:tail-indentation  [γ #f]) ; angle in degrees 
  (flipy (harpoon-up #:length            l
                     #:length-ratio      r 
                     #:head-angle        α
                     #:flank-indentation β
                     #:tail-indentation  γ)))

; return the curve for the arrow head placed at the end of the curve c
(define (place-arrow-head c 
                          #:length            [l #f] 
                          #:length-ratio      [r #f] 
                          #:head-angle        [α #f]  ; angle in degrees
                          #:flank-indentation [β #f]  ; angle in degrees  (todo: better word?)
                          #:tail-indentation  [γ #f]
                          #:head              [head arrow-head])
  (def n (curve-length c))
  (defm (and tip (pt tipx tipy)) (point-of c n))
  (def d (direction-of c n))
  ((shifted tipx tipy)
   (rotated (if (equal? d (vec 0 0)) 0 (angle d)))
   (shifted (- (ahlength)) 0) 
   (head #:length            l
         #:length-ratio      r
         #:head-angle        α
         #:flank-indentation β
         #:tail-indentation  γ)))

(define (reverse-arrow-head #:length            [l #f] 
                            #:length-ratio      [r #f] 
                            #:head-angle        [α #f]  ; angle in degrees
                            #:flank-indentation [β #f]  ; angle in degrees  (todo: better word?)
                            #:tail-indentation  [γ #f])
  (unless l (set! l (ahlength)))
  (shifted l 0 (flipx (arrow-head #:length            l
                                  #:length-ratio      r
                                  #:head-angle        α
                                  #:flank-indentation β
                                  #:tail-indentation  γ))))


(def (draw-arrow c 
                 #:length            [l #f] 
                 #:length-ratio      [r #f] 
                 #:head-angle        [α #f]  ; angle in degrees
                 #:flank-indentation [β #f]  ; angle in degrees  (todo: better word?)
                 #:tail-indentation  [γ #f]
                 #:fill-head         [fill-head? #t]
                 #:fill-tail         [fill-tail? #t]
                 #:head              [head arrow-head]
                 #:tail              [tail #f])
  
  (def the-head (place-arrow-head c 
                                  #:length            l
                                  #:length-ratio      r
                                  #:head-angle        α
                                  #:flank-indentation β
                                  #:tail-indentation  γ
                                  #:head              head))
  (def the-tail (if tail
                    (place-arrow-head (curve-reverse c)
                                      #:length            l
                                      #:length-ratio      r
                                      #:head-angle        α
                                      #:flank-indentation β
                                      #:tail-indentation  γ
                                      #:head              tail)
                    empty-curve))
  (draw c 
        (if fill-head? (filldraw the-head) (draw the-head))
        (if fill-tail? (filldraw the-tail) (draw the-tail))))


(def (draw-double-arrow c 
                        #:length            [l #f] 
                        #:length-ratio      [r #f] 
                        #:head-angle        [α #f]  ; angle in degrees
                        #:flank-indentation [β #f]  ; angle in degrees  (todo: better word?)
                        #:tail-indentation  [γ #f])
  (draw (draw-arrow c 
                    #:length            l
                    #:length-ratio      r
                    #:head-angle        α
                    #:flank-indentation β
                    #:tail-indentation  γ)
        (draw-arrow (curve-reverse c) 
                    #:length            l
                    #:length-ratio      r
                    #:head-angle        α
                    #:flank-indentation β
                    #:tail-indentation  γ)))

(def (arrow-head-mp c) ; plain
  ; Old MetaPost Style - not too pretty
  (def tip (point-of c (curve-length c)))
  (def stem (cut-before c ((scaled (ahlength)) unitcircle)))
  (def side1 ((rotated    (ahangle))  stem))
  (def side2 ((rotated (- (ahangle))) stem))
  (draw side1 side2 (curve (point-of side1 0) -- (point-of side2 0))))

