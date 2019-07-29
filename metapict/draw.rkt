#lang racket/base
;;; Draw

(provide draw           ; render arguments, then superimpose them
         draw*          ; draw a list of curves (or other drawables) on a new pict
         draw-dot       ; draw a dot; the size is determined by the current pen size
         fill           ; fill curves with brush using winding rule
         eofill         ; fill curves with brush using even-odd rule
         filldraw       ; fill closed curve with fill-color, then draw curve ontop
         clipped        ; draw the part of a pict that is inside a closed curve
         label->pict    ; convert label to pict
         for/draw       ; comprehension that draws each iteration on top of the old
         for*/draw
         ; for/draw/fold
         )

(require (for-syntax racket/base)
         racket/draw racket/class racket/format racket/match racket/list pict/convert
         "bez.rkt" "curve.rkt" "def.rkt" "device.rkt" "pen-and-brush.rkt"
         "pict-lite.rkt" "structs.rkt" "parameters.rkt" "pict.rkt" "pt-vec.rkt" "color.rkt")

; draw : list-of-drawables -> pict
(define (draw . cs)
  (smoothed
   (match cs
     ['() (blank (curve-pict-width) (curve-pict-height))]
     [_   (apply cc-superimpose
                 (for/list ([c (in-list cs)])
                   (match c
                     [(curve: #f '())       (blank (curve-pict-width) (curve-pict-height))]
                     [(? curve:?)           (curve->pict c)]
                     [(? pict?)             c]
                     [(? pt?)               (draw-dot c)]
                     [(? bez?)              (curve->pict (curve: #f (list c)))]
                     [(? label?)            (label->pict c)]
                     [(? node? n)           ((node-convert n) n)]
                     [(? edge? n)           ((edge-convert n) n)]
                     ;[(? pict-convertible?) (pict-convert c)]
                     [#f                    (blank (curve-pict-width) (curve-pict-height))]
                     [else            (error 'combine (~a "curve or pict expected, got " c))])))])))

(define (draw* cs) 
  (apply draw (filter values cs)))

(define (fill . cs)
  (smoothed (curves->filled-pict cs #:rule 'winding)))

(define (eofill . cs)
  (smoothed (curves->filled-pict cs  #:rule 'odd-even)))

(define (filldraw c [fill-color #f] [draw-color #f])
  (cond
    [(not fill-color) (draw c (fill c))] ; draw first, fill later
    [(not draw-color) (draw (brush fill-color (fill c)) c)]
    [else             (draw (brush fill-color (fill c))
                            (color draw-color (draw c)))]))

(define (draw-dot pos)
  (filldraw (curve pos))) ; todo dot color ?

(define (curves->pict cs)
  (def w (curve-pict-width))
  (def h (curve-pict-height))
  (def T (stdtrans (curve-pict-window) w h))
  (dc (λ (dc dx dy) 
        (def old-brush     (send dc get-brush))
        (def old-smoothing (send dc get-smoothing))
        (send dc set-smoothing 'smoothed)
        (send dc set-brush (find-white-transparent-brush))
        ; todo: use draw-bezs (takes care of t and pt)        
        (for ([c cs])
          (defm (curve closed? bs) c)
          (def Tp (bezs->dc-path (map T bs)))
          (send dc draw-path Tp dx dy))
        (send dc set-brush old-brush)
        (send dc set-smoothing old-smoothing))
      w h))

(define (curve->pict c)
  (curves->pict (list c)))

(define (clipped p c) 
  ; clip the picture p using a region given by the curve c
  (def w (curve-pict-width))
  (def h (curve-pict-height))
  (def T (stdtrans (curve-pict-window) w h))
  (dc (λ (dc dx dy) 
        (def old-smoothing (send dc get-smoothing))
        (send dc set-smoothing 'smoothed)
        (def old-region (send dc get-clipping-region))
        (def reg (new region% [dc dc]))
        (defm (curve closed? bs) c)
        (def Tp (bezs->dc-path (map T bs)))
        (send reg set-path Tp dx dy)
        (when old-region
          (send reg intersect old-region))
        (send dc set-clipping-region reg)
        (draw-pict p dc dx dy)
        
        (send dc set-clipping-region old-region)
        (send dc set-smoothing old-smoothing))
      w h))

(def black-solid-brush (send the-brush-list find-or-create-brush "black" 'solid))
(define (set-transparent-pen dc) (send dc set-pen "black" 1 'transparent))

(define (curve->filled-pict c #:draw-border? [draw-border? #f])
  (def w (curve-pict-width))
  (def h (curve-pict-height))
  (def T (stdtrans (curve-pict-window) w h)) ; logical -> device coords
  (dc (λ (dc dx dy)
        (def old-smoothing (send dc get-smoothing))
        (send dc set-smoothing 'smoothed)
        (def b (send dc get-brush))
        (def p (send dc get-pen))
        (when (use-default-brush?) 
          ; this overrides the brush in a new dc (it is white !?!)
          (send dc set-brush black-solid-brush))
        (unless draw-border?
          (set-transparent-pen dc))
        (defm (curve closed? bs) c)
        (unless (empty? bs)
          ; transform to device coordinates:
          (def Tp (bezs->dc-path (map T bs)))
          ; todo: use draw-bezs (takes care of t and pt)
          ; see PostScript reference for fill-styles 'winding and 'odd-even
          ; 'winding is the default
          (send dc draw-path Tp dx dy)) ; todo add fill-style 'odd-even or 'winding
        (send dc set-brush b)
        (send dc set-pen p)
        (send dc set-smoothing old-smoothing))
      w h))

(define (curves->filled-pict cs 
                             #:draw-border? [draw-border? #f]
                             #:rule [rule 'winding])
  (def w (curve-pict-width))
  (def h (curve-pict-height))
  (def T (stdtrans (curve-pict-window) w h)) ; logical -> device coords
  (dc (λ (dc dx dy)
        (def old-smoothing (send dc get-smoothing))
        (send dc set-smoothing 'smoothed)
        (def b (send dc get-brush))
        (def p (send dc get-pen))
        (when (use-default-brush?) 
          ; this overrides the brush in a new dc (it is white !?!)
          (send dc set-brush black-solid-brush))
        (unless draw-border?
          (set-transparent-pen dc))
        (def paths (new dc-path%))
        (for ([c (in-list cs)])
          (defm (curve closed? bs) c)
          (unless (empty? bs)
            ; transform to device coordinates:
            (def Tp (bezs->dc-path (map T bs)))
            (send Tp close)
            (send paths append Tp))
          ; todo: use draw-bezs (takes care of t and pt)
          ; see PostScript reference for fill-styles 'winding and 'odd-even
          ; 'winding is the default
          ; todo add fill-style 'odd-even or 'winding
          )
        (send dc draw-path paths dx dy rule)
        (send dc set-brush b)
        (send dc set-pen p)
        (send dc set-smoothing old-smoothing))
      w h))

(define (filled-curve p #:draw-border? [draw-border? #t]) ; path -> pict
  (def bs (flatten (segments->bezs (make-choices p))))
  (def dc-paths (map bez->dc-path bs))
  (defv (width height)
    (for/fold ([width 0] [height 0]) ([dc-path dc-paths])
      (defv (left top lw lh) (send dc-path get-bounding-box))
      (defv (w h) (values (+ left lw) (+ top lh)))
      (values (max w width) (max h height))))
  (dc (λ (dc dx dy)
        (def b (send dc get-brush))
        (def p (send dc get-pen))
        (def s (send dc get-smoothing))
        (send dc set-smoothing 'smoothed)
        (send dc set-brush (send the-brush-list find-or-create-brush
                                 (send p get-color) 'solid))
        (unless draw-border?
          (send dc set-pen "black" 1 'transparent))
        (def closed-dc-path (new dc-path%))
        (for ([dc-path dc-paths])
          (send closed-dc-path append dc-path))
        (send closed-dc-path close)
        (send dc draw-path closed-dc-path dx dy)
        (send dc set-brush b)
        (send dc set-pen p)
        (send dc set-smoothing s))
      width height))

(define (label->pict l) ; win w h [pt #f]) ; path -> pict 
  (def pw (curve-pict-width))
  (def ph (curve-pict-height))
  (def T (stdtrans (curve-pict-window) pw ph))
  (defm (label p? pos placement) l)
  (def p (if (pict? p?) p?
             (with-font (current-label-font) (text p?))))
  (defv (w h) (values (pict-width p) (pict-height p)))
  (dc (λ (dc dx dy)
        (def s (send dc get-smoothing))
        (send dc set-smoothing 'smoothed)
        (defm (pt x0 y0) (T pos))
        (defv (-w -h) (values (- w) (- h)))
        (defv (-w/2 -h/2) (values (/ -w 2) (/ -h 2)))
        (def ε (label-offset))
        (def -ε (- ε))
        (defm (vec Δx Δy)
          (match placement
            [(rt)   (vec+ (vec 0    -h/2) (vec  ε  0))]
            [(lft)  (vec+ (vec -w   -h/2) (vec -ε  0))]
            [(bot)  (vec+ (vec -w/2  0)   (vec  0  ε))]
            [(top)  (vec+ (vec -w/2 -h)   (vec  0 -ε))]
            [(cnt)  (vec+ (vec -w/2 -h/2) (vec  0  0))]            
            [(lrt)  (vec+ (vec  0    0)   (vec  ε  ε))]   ; lft is +
            [(ulft) (vec+ (vec -w   -h)   (vec -ε -ε))]            
            [(llft) (vec+ (vec -w    0)   (vec -ε  ε))]
            [(urt)  (vec+ (vec  0   -h)   (vec  ε -ε))]                        
            [else   (error 'label->pict (~a "internal error, expected a placement:" placement))]))
        (draw-pict p dc (+ x0 Δx dx) (+ y0 Δy dy))
        (send dc set-smoothing s))
      pw ph)) ; ?


(define-syntax (for/draw stx)
  (syntax-case stx ()
    [(_ clauses . defs+exprs)
     (with-syntax ([original stx])
       #'(for/fold/derived original ([drawing (draw)]) clauses
                           (draw drawing (let () . defs+exprs))))]))

(define-syntax (for*/draw stx)
  (syntax-case stx ()
    [(_ clauses . defs+exprs)
     (with-syntax ([original stx])
       #'(for*/fold/derived original ([drawing (draw)]) clauses
                            (draw drawing (let () . defs+exprs))))]))

#;(define-syntax (for/draw/fold stx)
    (syntax-case stx ()
      [(_ (id+init-expr ...) clauses . defs+exprs)
       (with-syntax ([original stx])
         #'(for/fold/derived original ([drawing (draw)] id+init-expr ...) clauses
                             (draw drawing (let () . defs+exprs))))]))

