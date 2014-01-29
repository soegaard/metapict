#lang racket

; General pict utilities 

(provide
 pencolor    ; use the pen color (doesn't affect brush)
 penwidth    ; use the pen width (alias for linewidth)
 penscale    ; scale the penwidth
 penstyle    ; use the pen style
 pencap      ; use the pen cap
 penjoin     ; use the pen join
 
 brushcolor  ; use the brush color
 brushstyle  ; use the brush style

 dashed      ; use the pen style long-dash
 dotted      ; use the pen style dotted
 
 save-pict-as-png ; save the pict in a png-file
 margin      ; inset with arguments swapped 
 )

(require pict racket/draw (for-syntax syntax/parse) "def.rkt")

(define (dashed p) (penstyle 'long-dash p))
(define (dotted p) (penstyle 'dot p))

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
     #'(define (name arg ... pict)
         (unless (pict? pict)
           (raise-arguments-error 'name "pict expected" "pict" pict))           
         (dc (lambda (dc x y)
               (let ([old-pen (send dc get-pen)])
                 (def new-pen (let () expr ...))
                 (send dc set-pen new-pen)
                 (draw-pict pict dc x y)
                 (send dc set-pen old-pen)))
             (pict-width pict) (pict-height pict)))]))

(define-penop pencolor (color pict) p
  (send the-pen-list find-or-create-pen 
        color (send p get-width) (send p get-style) 
        (send p get-cap) (send p get-join)))

(define-penop penwidth (width pict) p
  (send the-pen-list find-or-create-pen 
        (send p get-color) width (send p get-style)
        (send p get-cap) (send p get-join)))

(define-penop penscale (factor pict) p
  (send the-pen-list find-or-create-pen 
        (send p get-color) (* factor (send p get-width)) (send p get-style)
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
    [(_ name (arg ... pict) old-brush
        expr ...)
     #'(define (name arg ... pict)
         (dc (lambda (dc x y)
               (let ([old-brush (send dc get-brush)])
                 (def new-brush (let () expr ...))
                 (send dc set-brush new-brush)
                 (draw-pict pict dc x y)
                 (send dc set-brush old-brush)))
             (pict-width pict) (pict-height pict)))]))

(define-brushop brushcolor (color pict) b
  (send the-brush-list find-or-create-brush
        color (send b get-style)))

(define-brushop brushstyle (style pict) b
  (send the-brush-list find-or-create-brush
        (send b get-color) style))

(define (save-pict-as-png filename pict)
  (send (pict->bitmap pict)
        save-file filename 'png))

(define (margin n p) (inset p n))

