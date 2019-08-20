#lang racket/base
(require racket/contract/base racket/match
         "save-pdf.rkt" "font.rkt" "text.rkt" "gradient.rkt" "pt-vec.rkt")
;;;
;;; IMPORTANT NOTE
;;;   Remember that to set the smoothing mode to 'smoothed if you implement
;;;   new picts constructors, that use coordinates.

(provide (all-from-out "font.rkt" "text.rkt" "gradient.rkt"))
; General pict utilities
(provide
 ; color       ; use color for both the pen and brush color
 pencolor      ; use the pen color (doesn't affect brush)
 penwidth      ; use the pen width (alias for linewidth)
 penscale      ; scale the penwidth
 penstyle      ; use the pen style
 pencap        ; use the pen cap
 penjoin       ; use the pen join
 pen           ; use the pen
 
 brush         ; use the brush
 brushcolor    ; use the brush color
 brushstyle    ; use the brush style
 brushstipple  ; use the brush stipple
 brushgradient ; use the brush gradient
;  brushshade

 dashed        ; use the pen style long-dash
 dotted        ; use the pen style dotted

 text-color    ; use the color as the text foreground color
 text-background-color ; use the color as background color when text mode is solid
 text-mode     ; use the mode 'solid or 'transparent
 
 margin        ; inset with arguments swapped
 smoothed      ; use smoothing-mode 'smoothed
 aligned       ; use smoothing-mode 'aligned
 
 pict-size     ; returns width and height
 
 (contract-out 
  [scale (-> number? pict? pict?)]) ; pict:scale with arguments swapped

   save-pict     ; save pict to file, default is png, other formats include jpeg

 )

(require (for-syntax racket/base syntax/parse)
         racket/draw racket/class racket/format
         "pict-lite.rkt" "def.rkt" "color.rkt" "structs.rkt" "device.rkt" "parameters.rkt")

(define (dashed p) (penstyle 'long-dash p))
(define (dotted p) (penstyle 'dot p))

; color is stored in colorizer due to the match-expander in color.rkt
(colorizer (case-lambda [(  c p) (pencolor c (brushcolor c p))]
                        [(f c p) (let ([c (color* f c)]) (pencolor c (brushcolor c p)))]
                        [else    (error 'color "expected (color [f] c p)")]))
              

;; (define-penop name (arg ... pict) old-pen expr ...)
;; Defines a function name such that 
;;   (name arg ... pict)
;; Draws the pict with a the pen resulting from
;; evaluating (let () expr ...) in an environment where
;; old-pen is bound the current pen.
(define-syntax (define-penop stx)
  (syntax-parse stx
    [(_ name (arg ... pict) old-pen
        expr ...)
     #`(define (name arg ... pict)
         (unless (pict? pict)
           (raise-arguments-error 'name "pict expected" "pict" pict))           
         (dc #,(syntax/loc stx
                        (lambda (dc x y)
                          (let ([old-pen (send dc get-pen)])
                            (def new-pen (let () expr ...))
                            (send dc set-pen new-pen)
                            (draw-pict pict dc x y)
                            (send dc set-pen old-pen))))
                    (pict-width pict) (pict-height pict)))]))

(define-penop pen (new-pen pict) b
  new-pen)

(define-penop pencolor (color pict) p
  (send the-pen-list find-or-create-pen 
        color (send p get-width) (send p get-style) 
        (send p get-cap) (send p get-join)))

(define-penop penwidth (width pict) p
  (send the-pen-list find-or-create-pen 
        (send p get-color) (min 255 width) (send p get-style)
        (send p get-cap) (send p get-join)))

(define-penop penscale (factor pict) p
  (send the-pen-list find-or-create-pen 
        (send p get-color) (min (* factor (send p get-width)) 255) (send p get-style)
        (send p get-cap) (send p get-join)))

(define-penop penstyle (style pict) p
  ; styles: 'transparent 'solid 'xor 'hilite 'dot 'long-dash 'short-dash 'dot-dash
  ;         'xor-dot 'xor-long-dash 'xor-short-dash 'xor-dot-dash
  (send the-pen-list find-or-create-pen 
        (send p get-color) (send p get-width) style
        (send p get-cap) (send p get-join)))

(define-penop pencap (cap pict) p
  ; draw pict with a pen with the given cap (round, projecting, or, butt)
  (when (eq? cap 'square) (set! cap 'projecting))
  (send the-pen-list find-or-create-pen 
        (send p get-color) (send p get-width) (send p get-style)
        cap (send p get-join)))

(define-penop penjoin (join pict) p
  ; draw pict with a pen with the given join (round, bevel, or, miter)
  (send the-pen-list find-or-create-pen 
        (send p get-color) (send p get-width) (send p get-style)
        (send p get-cap) join))

;; (define-brushop name (arg ... pict) old-brush expr ...)
;; Defines a function name such that 
;;   (name arg ... pict)
;; Draws the pict with the brush resulting from
;; evaluating (let () expr ...) in an environment where
;; old-brush is bound the current brush.

(define-syntax (define-brushop stx)
  (syntax-parse stx
    [(_ name (arg ... pict x y) old-brush the-dc
        expr ...)
     #'(define (name arg ... pict)
         (unless (pict? pict)
           (raise-arguments-error 'name "pict expected" "pict" pict))
         (dc (lambda (the-dc x y)
               (parameterize ([use-default-brush? #f])
                 (let ([old-brush (send the-dc get-brush)])
                   (def new-brush (let () expr ...))
                   (when (eq? new-brush #t)
                     (set! new-brush old-brush))
                   (send the-dc set-brush new-brush)
                   (draw-pict pict the-dc x y)
                   (send the-dc set-brush old-brush))))
             (pict-width pict) (pict-height pict)))]))

(define-brushop brush (new-brush pict x y) b dc
  (cond [(eq? new-brush #t)       #t] ; use existing
        [(is-a? new-brush brush%) new-brush]
        [(is-a? new-brush color%) (new brush% [color new-brush] [style 'solid])]
        [(string? new-brush)      (new brush% [color (make-color* new-brush)]
                                       [style 'solid])]        
        [else (error 'brush (~a "expected a brush%, a color% or a color string, got: "
                                new-brush))]))


(define-brushop brushcolor (color pict x y) b dc
  (send the-brush-list find-or-create-brush
        (make-color* color) (send b get-style)))

(define-brushop brushstyle (style pict x y) b dc
  (send the-brush-list find-or-create-brush
        (send b get-color) style))

(define-brushop brushstipple (stipple pict x y) b dc
  (new brush% 
       [color          (send b get-color)]
       [style          (send b get-style)]
       [stipple        stipple]
       [gradient       (send b get-gradient)]
       [transformation (send b get-transformation)]))


(require "trans.rkt")
(define (brushgradient g pict #:height-factor [height-factor #f])
  ; g is a gradient (with points in logical coordinates)
  (unless (pict? pict)
    (raise-arguments-error 'name "pict expected" "pict" pict))

  (defm (or (and (raw-linear-gradient color-stops p0 p1 hf) r0 r1)
            (raw-radial-gradient color-stops p0 r0 p1 r1 hf)) g)
  
  ; Note: It is important these are placed outside the (dc ...)
  ;       We need the values of the parameters at the pict is created.
  ;       So ... don't use  define-brushop
  (def w (curve-pict-width))
  (def h (curve-pict-height))
  (def win (curve-pict-window))
  ; The transformation T is from logical coordinates into user coordinates.
  ; I.e. from coordinates used in MetaPict into coordinates used by picts.
  (def T (stdtrans win w h)) ; logical -> user coordinates
  (dc (λ (the-dc x y)
        ; The drawing context contains the tranformation used
        ; to go from user coordinates into device coordinates.
        ; Transformation U   : user -> device
        (def raw-transformation (send the-dc get-transformation))
        (def o compose-trans)
        (def UD (match raw-transformation
                  [(vector (vector xx xy yx yy x0 y0) ; initial
                           x-origin
                           y-origin
                           a ; x-scale
                           b; y-scale
                           rot)

                   ; Order the transformations are applied
                   ;   1. initial
                   ;   2. origin
                   ;   3. scaling
                   ;   4. rotation
                   (o (trans xx yx xy yy x0 y0)            ; initial
                      (o (trans 1 0 0 1 0 0)
                         ; not quite sure why origin must be ignored :-(
                         ;(trans 1 0 0 1 x-origin y-origin) ; origin
                         (o (trans a 0 0 b 0 0)            ; scaling
                            (rotatedd rot))))]))           ; rotation
        (set! T (compose-trans UD T))
        ; !! now T is from  logical to device coordinates
        (parameterize ([use-default-brush? #f])
          (let ([old-brush (send the-dc get-brush)])
            (def new-brush
              (let ()
                ; find points in (dc) device coordinates
                (define-values (x0 y0 x1 y1) ; device coords
                  (match* ((T p0) (T p1)) ; (T p0) transform from logical to device
                    [((pt x0 y0) (pt x1 y1))
                     (values x0 y0 x1 y1)]))
                  
                ; width and height (in device coords)
                (def width  (- x1 x0))
                (def height (- y1 y0))
                (when (= x0 x1)    (set! x1 (+ x1 0.001))) 
                (when (= y0 y1)    (set! y1 (+ y1 0.001)))
                (when (= width 0)  (set! width  0.001))
                (when (= height 0) (set! height 0.001))

                ; This transformation corresponds to the rectangle bounded by p0 p1
                (def S (stdtrans (window x0 x1 y0 y1) width height)) ; pattern coords -> user
                           
                
                ; Time to setup the gradient
                (define P ((inverse S) T)) ; logical -> pattern
                (define a-gradient (convert-gradient g P))
                ; if the gradient is a radial-gradient, the radius was
                ; calculated using the "x-radius".
                ; in an ellipse we need to scale the y-axis

                ; The Q transformation is from pattern space to pattern space.
                (def Q (shifted (P p0)
                                (yscaled (or height-factor hf) ; height factor
                                         (shifted (pt- (P p0))))))
                
                (def transformation (trans->transformation (S Q)))
                
                (define b old-brush)
                (new brush% 
                     [color          (send b get-color)]   ; ignored
                     [style          (send b get-style)]   ; ignored when a gradient is present
                     [stipple        (send b get-stipple)] ; ignored
                     [gradient       a-gradient]
                     [transformation transformation])))
            ; install brush, draw pict and reinstall old brush
            (send the-dc set-brush new-brush)            
            (draw-pict pict the-dc x y)
            (send the-dc set-brush old-brush))))
      (pict-width pict) (pict-height pict)))


;; text-color : color pict -> pict
;;   Produces a pict like p, but using the color c as the text foreground color.
(define (text-color c p)
  ; todo: check c is a color
  (dc (lambda (dc x y)
        (let ([old-color (send dc get-text-foreground)])
            (def new-color c)
            (send dc set-text-foreground new-color)
            (draw-pict p dc x y)
            (send dc set-text-foreground old-color)))
      (pict-width p) (pict-height p)))

;; text-background-color : color pict -> pict
;;   Produces a pict like p, but using the color c as the text background color.
;;   The text background color is only used when text mode is 'solid.
(define (text-background-color c p)
  ; todo: check c is a color
  (dc (lambda (dc x y)
        (let ([old-color (send dc get-text-background)])
            (def new-color c)
            (send dc set-text-background new-color)
            (draw-pict p dc x y)
            (send dc set-text-background old-color)))
      (pict-width p) (pict-height p)))

;; text-mode : mode pict -> pict
;;   Produces a pict like p, but using the mode ('solid or 'transparent) as the text mode.
(define (text-mode m p)
  (unless (or (eq? m 'solid) (eq? m 'transparent))
    (raise-arguments-error 'text-mode "mode ('solid or 'transparent) expected" "mode" m))
  (dc (lambda (dc x y)
        (let ([old-mode (send dc get-text-mode)])
            (def new-mode m)
            (send dc set-text-mode new-mode)
            (draw-pict p dc x y)
            (send dc set-text-mode old-mode)))
      (pict-width p) (pict-height p)))



;; smoothed : pict -> pict
;;  Produces a pict like `p`, but that always draws in 'smoothed mode
(define (smoothed p)
  ; (define draw-p (make-pict-drawer p))
  (def p2
    (dc (lambda (dc x y)
          (def s (send dc get-smoothing))
          (send dc set-smoothing 'smoothed)
          ; (draw-p dc x y)
          (draw-pict p dc x y)
          (send dc set-smoothing s))
        (pict-width p)
        (pict-height p)))
  (make-pict (pict-draw p2)
             (pict-width p)
             (pict-height p)
             (pict-ascent p)
             (pict-descent p)
             (list (make-child p 0 0 1 1 0 0))
             #f
             (pict-last p)))

;; aligned : pict -> pict
;;  Produces a pict like `p`, but that always draws in 'aligned mode
(define (aligned p)
  ; (define draw-p (make-pict-drawer p))
  (def p2
    (dc (lambda (dc x y)
          (def s (send dc get-smoothing))
          (send dc set-smoothing 'aligned)
          ; (draw-p dc x y)
          (draw-pict p dc x y)
          (send dc set-smoothing s))
        (pict-width p)
        (pict-height p)))
  (make-pict (pict-draw p2)
             (pict-width p)
             (pict-height p)
             (pict-ascent p)
             (pict-descent p)
             (list (make-child p 0 0 1 1 0 0))
             #f
             (pict-last p)))

(define (save-pict filename pict [type 'png])
  (define (save-bitmap type)
    (send (pict->bitmap pict) save-file filename type))
  (case type
    [(png jpeg xbm xpm bmp) (save-bitmap type)]
    ;[(jpg)                  (save-bitmap 'jpeg)] ; jpeg not supported as alpha appears as black in jpg.
    [(pdf)                  (save-pict-as-pdf pict filename)]
    [else (error 'save-pict (~a "expected one of: png pdf xbm xpm bmp, got: " type))]))

(define (margin n p) (inset p n))
(define (scale  n p) (pict:scale p n))

(define (pict-size p) (values (pict-width p) (pict-height p)))

