#lang racket/base
(provide complete-histogram-from-observations)

(require racket/format
         racket/match
         racket/list
         
         math/statistics
         math/distributions

         "arrow.rkt"
         "axis.rkt"
         "color.rkt"
         "curve.rkt"
         "def.rkt"
         "device.rkt"
         "domain.rkt"
         "draw.rkt"
         (except-in "label.rkt" open)
         "parameters.rkt"
         "pict.rkt"
         "trans.rkt"
         "window.rkt"
         (except-in "structs.rkt" open)
         "system.rkt"
         "skyline.rkt"
         "slices.rkt")

(define (box s x0 x1 h [y0 0])
  ; draw a box in system s, where (x0,y0) and (x1,y0+h) are opposite corners
  (define A (point s x0 (+ y0 0)))
  (define B (point s x1 (+ y0 0)))
  (define C (point s x1 (+ y0 h)))
  (define D (point s x0 (+ y0 h)))
  (defm (list a b c d) (map point->pt (list A B C D)))
  (curve a -- b -- c -- d -- a))

(define (bounds->domains bs)
  ; convert a list of bounds into a list of domains
  (match bs
    [(list* x0 x1 xs) (cons (closed x0 x1) (bounds->domains (cons x1 xs)))]
    [_                '()]))

(define (bounds->gap-sizes bs)
  ; Convert a list of bounds bs, to a list of gap sizes.
  ; The gap size of [a;b] is b-a.
  (match bs
    [(list* x0 x1 xs) (cons (- x1 x0) (bounds->gap-sizes (cons x1 xs)))]
    [_                '()]))

(define (bounds->minimal-gap-size bs)
  ; Compute the min_b gapsize(b).
  (apply min (bounds->gap-sizes bs)))


(define (from d)
  ; start point of interval domain
  (match d
    [(domain (list (domain-interval ac a b bc))) a]
    [(domain-interval ac a b bc)                 a]
    [_ (error 'from (~a "the input was an interval" d))]))

(define (to d)
  ; end point of interval domain
  (match d
    [(domain (list (domain-interval ac a b bc))) b]
    [(domain-interval ac a b bc)                 b]
    [_ (error 'from (~a "the input was an interval" d))]))

(define (histogram-block-height s bounds block-area)
  (def a2           (second-axis s))
  (def gap-sizes    (bounds->gap-sizes bounds))
  (def min-gap-size (apply min gap-sizes))
  (def height       (/ block-area min-gap-size)) ; a2 coordinates
  height)
  
(define (histogram-block s bounds block-area max-weight block-text/pict)
  ; The histogram block is the block drawn as legend to show the size.
  ; block-area determines the size of the block
  ; If block-areas is, say, 5%, then make a text/pict that says "5%" as label.
  ; The label is shown below the block.  
  ; s = coordinate system - in which the block is draw
  ; (displayln (list 'histogram-block bounds block-area max-weight))

  
  (def a2           (second-axis s))
  (def gap-sizes    (bounds->gap-sizes bounds))
  (def min-gap-size (apply min gap-sizes))
  (def height       (/ block-area min-gap-size)) ; a2 coordinates
  (def xn           (+ (last bounds)  (* 0.5 min-gap-size)))
  (def xn+          (+ (last bounds)  (* 1.5 min-gap-size)))
  (def y+           max-weight)
  (def y            (- (/ max-weight min-gap-size) height))
  (draw (box s xn xn+ height y)
        (label-bot block-text/pict
                   (point->pt (point s (* .5 (+ xn xn+)) y)))))
                         

(define (simple-histogram s bounds weights #:y-scale [y-scale 1.0])
  ; Here s is a system in which to draw the histogram.
  ; The height of a box is  scale * weight/total_weight.
  ; Use 100 as a scale in order for the ticks on y to from 0 to 100.

  ; Typically the weights will be absolute frequencies, so the
  ; height will be proportional to the relative frequency.

  ; The length of bounds is one greater than the length of the  weights.
  (define total-weight (for/sum  ([w weights]) w))
  (define domains      (bounds->domains bounds))
  (define heights      (for/list ([w weights] [d domains])
                         (define len (domain-length d))
                         (* 1. (/ w total-weight len))))
  
  (for/draw ([d domains] [h heights])
    (brushcolor "white" (filldraw (box s (from d) (to d) h)))))

(define (skyline-histogram s bounds weights #:y-scale [y-scale 1.0])
  ; This alternative to `simple-histogram` avoids drawing lines twice.
  ; We first fill the rectangles with white.
  ; This is done to erase the dashed horizontal lines.  
  
  ; Here s is a system in which to draw the histogram.
  ; The height of a box is  scale * weight/total_weight.
  ; Use 100 as a scale in order for the ticks on y to from 0 to 100.

  ; Typically the weights will be absolute frequencies, so the
  ; height will be proportional to the relative frequency.

  ; The length of bounds is one greater than the length of the  weights.
  (define total-weight (for/sum  ([w weights]) w))
  (define domains      (bounds->domains bounds))
  (define heights      (for/list ([w weights] [d domains])
                         (define len (domain-length d))
                         (* 1. (/ w total-weight len))))
  (define input        (for/list ([h heights] [d domains])
                         (list (from d) h (to d))))
  (define key-points   (skyline input))
  (define lines        (skyline->lines key-points))
  (define roofs        (extract-every lines 2))
  (define walls        (extract-every (rest lines) 2))

  ;; Here `walls` consists of the vertical lines in a skyline.
  ;;   1) The lines needs to be extended to the y-axis.
  ;;   2) For two adjacent boxes with the same height, the middle wall is missing.

  ; (define x-boundaries  (map from domains)) ; rightmost x not included here
  (define missing-walls (let loop ([xs bounds] [roofs roofs])
                          (cond
                            [(null? xs)    '()]
                            [(null? roofs) '()]
                            [else (define x    (first xs))
                                  (define roof (first roofs))
                                  (match-define (list (list x0 y0) (list x1 y1)) roof) ; y0=y1 here
                                  (cond
                                    [(< x0 x x1) (cons (list (list x y0) (list x 0))
                                                       (loop (cdr xs) roofs))]
                                    [(<= x x0)   (loop (cdr xs) roofs)]                                    
                                    [else        (loop xs (cdr roofs))])])))

  (pencap 'butt
          (draw
           ; Todo: Instead of filling each building, one could fill the entire
           ;       skyline once - making the result svg more efficient (and shorter).

           ;; Fill inside with white
           (for/draw ([line roofs])
             (match-define (list (list x0 y0) (list x1 y1)) line)
             (color "white"
                    (fill (curve (pt x0 0) -- (pt x0 y0) -- (pt x1 y1) -- (pt x1 0) -- (pt x0 0)))))
           ;; Roofs
           (for/draw ([line roofs])
             (match-define (list (list x0 y0) (list x1 y1)) line)
             (curve (pt x0 y0) -- (pt x1 y1)))
           ;; Vertical lines
           (for/draw ([line walls])
             (match-define (list (list x0 y0) (list x1 y1)) line)
             (curve (pt x0 (max y0 y1)) -- (pt x0 0)))
           ;; Missing walls
           (for/draw ([line missing-walls])
             (match-define (list (list x0 y0) (list x1 y1)) line)
             (curve (pt x0 y0) -- (pt x1 y1))))))


(define (complete-histogram-from-observations
         observations
         #:bounds        [bounds        #f]
         #:minimum-bound [minimum-bound #f]
         #:maximum-bound [maximum-bound #f]         
         #:bin-size      [bin-size      #f] 
         #:bin-number    [bin-number    #f]
         #:lean-left?    [lean-left?    #f] ; cound an observation on boundary to the left?
         #:pict-size-x   [pict-size-x   300]
         #:pict-size-y   [pict-size-y   300]
         #:block-area    [block-area    0.05]
         #:block-label   [block-label   "5%"] ; could be a pict
         #:y-scale       [y-scale       1.0]
         #:y-axis?       [y-axis?       #f]   ; show second axis?
         #:y-grid?       [y-grid?       #f]   ; show horizontal grid lines?
         #:few-labels?   [few-labels?   #f]
         #:labels-at     [labels-at     #f])
  ;; Range of observations
  (define min-obs   (apply min observations))
  (define max-obs   (apply max observations))

  ;; 0. Bin Size
  
  ;; Given explicit bounds and no bin-size, use the minimal-gap-size
  (when (and bounds (not bin-size))
    (set! bin-size (bounds->minimal-gap-size bounds)))
  
  ;; 1. Determine Bounds
  (define min-bound (or minimum-bound
                        (and bounds
                             (apply min bounds))
                        min-obs))
  (define max-bound (or maximum-bound
                        (and bounds
                             (apply min bounds))
                        (and bin-size bin-number
                             (+ min-bound (* bin-number bin-size)))
                        (and bin-size ; bin-number missing, use max-obs and "round up"
                             (+ min-bound (* bin-size (ceiling (/ (- max-obs min-bound) bin-size)))))
                        (error 'complete-histogram-from-observations
                               "max-bound can't be computed: pass either explicit bounds or the bin-size to use")))  
  (set! bounds (or bounds
                   (let loop ([b min-bound] [i 1])
                     (if (>= b max-bound)
                         (list max-bound)
                         (cons b
                               (loop (+ min-bound (* i bin-size))
                                     (+ i 1)))))))

  ;; 2. Bin the samples
  
  (define xs           observations)
  (define n            (length observations))
  (define <<           (if lean-left? <= <))
  (define bins         (bin-samples bounds << xs (make-list n (/ 1. n))))
  (define weights      (for/list ([b bins]) (length (sample-bin-values b))))
  (define max-weight   (/ (apply max weights) n))

  (define min-gap      (bounds->minimal-gap-size bounds))

  ;; 3. Setup drawing area
  
  ;; Setup the drawing area
  (set-curve-pict-size pict-size-x pict-size-y)
  (define-values (xmin xmax) (values (- min-bound (* 2 min-gap))
                                     (+ max-bound (* 2 min-gap))))
  (define max-logical-y (+ 0.01 (/ max-weight min-gap)))
  (define below-logical-size  (* max-logical-y (/ 20 pict-size-y)))
  (define-values (ymin ymax) (values (- below-logical-size) max-logical-y))
  (def win (window xmin xmax ymin ymax))

  ;; 4. Draw axis, histogram and block

  (with-window win
    (def s  (system (pt 0 0) (vec 1 0) (vec 0 1)))
    (def a1 (first-axis  s))
    (def a2 (second-axis s))
    ; the horizontal gridlines have the block-height as distance
    (def block-height (histogram-block-height s bounds block-area))
    (draw
     ;; Horizontal grid lines
     (and y-grid? 
          (let ()
            (def x0 (- min-bound min-gap))
            (with-window (window (- xmin x0) (- xmax x0) ymin ymax)
              (for/draw ([y (in-range 0 1 block-height)])
                (def A (point->pt (point s 0    y)))
                (def B (point->pt (point s (* xmax) y)))
                (dashed (pencolor "gray" (draw (curve A -- B))))))))
     
     ;; Second Axis
     (and y-axis? 
          (let ()
            ; We need to draw the y-axis to the left of the first bar,
            ; therefore we move the window.
            (def x0 (- min-bound min-gap))
            (draw ; a2
                (with-window (window (- xmin x0) (- xmax x0) ymin ymax)
                  (draw ; (ticks a2 .1 #:size 3)             
                        (for/draw ([y (in-range 0.1 1.0 0.1)])
                          (tick-label a2 y #t)))))))
                    

     ; Histogram
     ; (simple-histogram s bounds weights #:y-scale y-scale)
     (skyline-histogram s bounds weights #:y-scale y-scale)

     ;; First Axis
     a1
     ;; Ticks
     (for/draw ([x bounds])
       (tick a1 x #:size 3))

     ; Labels
     (and labels-at
       (for/draw ([x labels-at])
         (tick-label a1 x #t)))
     (and few-labels?
       (draw (tick-label a1 (first bounds) #t)
             (tick-label a1 (last  bounds) #t)))
     (and (not (or labels-at few-labels?))
       (for/draw ([x bounds])
         (tick-label a1 x #t)))

     ; Area block
     (histogram-block s bounds block-area max-weight block-label)
     )))


;;;
;;; Example
;;;


; Observations are a list of samples (values) belonging some axis.
#;(define observations
  '(0.1 0.3 0.5 0.5 2.1 2.3
        3.4 3.5 3.6 3.6 3.7 4.1
        5.2 5.3
        6.2
        8.5 8.7 9.1 9.3 9.5))

#;(

(define X            (normal-dist 2.5 2))
(define observations (sample X 10000))

; The observations are put into bins using `bin-samples` from `math-statistics`.
; Note: The bins represent intervals of the type ]min, max] when lte? is a less-than-or-equa.
;       The first interval might be closed.
; Note: The bins represent intervals of the type [min, max[ when lte? is a less-than.
;       The last interval might be closed.

(define bounds       '(0 2 4 6 8 10 12))
(define min-bound    (apply min bounds))
(define max-bound    (apply max bounds))
(define min-gap      (bounds->minimal-gap-size bounds))
(define xs           observations)
(define n            (length observations))
(define bins         (bin-samples bounds <= xs (make-list n (/ 1. n))))
(define weights      (for/list ([b bins]) (length (sample-bin-values b))))
(define max-weight   (/ (apply max weights) n))
(displayln (list 'max-weight (* 1.0 max-weight)))
(define block-area   0.05)

(set-curve-pict-size 300 300)
(define-values (xmin xmax) (values (- min-bound min-gap)
                                   (+ max-bound (* 2 min-gap))))
(define-values (ymin ymax) (values xmin xmax))
(def win (window xmin xmax ymin ymax))
(with-window win
  (def s  (system (pt 0 0) (vec 1 0) (vec 0  (* (- ymax 1) (/ max-weight)))))
  (def a1 (first-axis s))
  (def a2 (second-axis s))
  (draw
   ; First Axis
   a1 
   (ticks a1 1    #:size (ypx 3))
   ; Second Axis
   ; a2
   #;(begin
       (let ()
         (defv (u v) (visible-range a2 win))
         (displayln (list 'visible-range u v))
         (displayln (tick-ordinates a2      ; axis
                                    0.1     ; axis units between ticks
                                    #:window win))
         
         #f)
       (ticks a2 .1   #:size (xpx 3))
       (for/draw ([y (in-range 0.0 1.0 0.1)])
         (tick-label a2 y #t)))
   ; Histogram
   (simple-histogram s bounds weights #:y-scale 1.0)
   ; Labels of bounds
   (for/draw ([x bounds])
     (tick-label a1 x #t))
   ; Area block
   (histogram-block s bounds block-area max-weight "5%")
   ))
)
