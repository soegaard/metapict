#lang racket/base
;;;
;;; Extents of inked area
;;;

; First order of business is to make a binding for cairo_recording_surface_ink_extents.

(provide crop
         crop/inked
         cairo_recording_surface_ink_extents)

(require ffi/unsafe
         ffi/unsafe/define
         racket/draw/unsafe/cairo
         racket/draw/unsafe/cairo-lib)

; The functions in draw/unsafe/cairo uses _ptr/immobile
; to prevent objects moving during garbage collection.
; We need an io version. For now we use (_ptr io _double),
; but that doesn't make the objects immovable (I think).
(define-fun-syntax _ptr/immobile
  (syntax-id-rules (_ptr/immobile o)
    [(_ptr/immobile o t) (type: _pointer
                                pre:  (malloc t 'atomic-interior)
                                post: (x => (ptr-ref x t)))]))

(define-ffi-definer define-cairo cairo-lib
  #:provide provide-protected)

(define-syntax-rule (_cfun . rest)
  (_fun #:lock-name "cairo-pango-lock" . rest))

;; void
;; cairo_recording_surface_ink_extents (cairo_surface_t *surface,
;;                                      double *x0,
;;                                      double *y0,
;;                                      double *width,
;;                                      double *height);

(define-cairo cairo_recording_surface_ink_extents
  (_cfun _cairo_surface_t
         (x0     : (_ptr io _double)) 
         (y0     : (_ptr io _double)) 
         (width  : (_ptr io _double)) 
         (height : (_ptr io _double)) 
         -> _void
         -> (values x0 y0 width height)))


;;;
;;; Cairo Recording DC
;;; 

; A cairo recording surface can be used to find bounding box
; of the the inked extents of a drawing.

; The method call (send dc get-inked-extents) will
; return four values: x y w and h.

; The point (x,y) is the upper, left corner of the inked extents
; and w and h are the width and height of the inked area.

(provide cairo-record-dc%)

; We need the same imports as in "svg-dc.rkt"

(require racket/class
         racket/draw/unsafe/cairo 
         racket/draw/private/syntax        ; "syntax.rkt"
         racket/draw/private/dc            ; "dc.rkt"
         racket/draw/private/local         ; local.rkt"
         )

;;;
;;; Backend for cairo-record-dc% 
;;; 

(define dc-backend%
  (class default-dc-backend%
    (init [(init-x0 x0)]
          [(init-y0 y0)]
          [(init-w width)]
          [(init-h height)])

    (unless (real? init-x0)
      (raise-type-error (init-name 'record-dc%) "non-real or #f" init-x0))
    (unless (real? init-y0)
      (raise-type-error (init-name 'record-dc%) "non-real or #f" init-y0))
    (unless (and (real? init-w) (not (negative? init-w)))
      (raise-type-error (init-name 'record-dc%) "nonnegative real or #f" init-w))
    (unless (and (real? init-h) (not (negative? init-h)))
      (raise-type-error (init-name 'record-dc%) "nonnegative real or #f" init-h))
    
    (define width  init-w)
    (define height init-h)
    (define x0     init-x0)
    (define y0     init-y0)
    
    (define s (cairo_recording_surface_create CAIRO_CONTENT_COLOR_ALPHA #f))
    (define c (and s (cairo_create s)))
    
    (when s (cairo_surface_destroy s))  ; decrease reference count

    (define/override (ok?) (and c #t))

    (define/override (get-cr) c)

    (def/override (get-size)
      (values width height))

    (define/override (end-cr)
      (cairo_surface_finish s)
      (cairo_destroy c)
      (set! c #f)
      (set! s #f))


    ; keep these?
    (define/override (get-pango font)
      (send font get-pango))

    (define/override (get-font-metrics-key sx sy)
      (if (and (= sx 1.0) (= sy 1.0))
          3
          0))

    (define/override (can-combine-text? sz)
      #t)
    

    (define/public (multiple-pages-ok?) #f)

    (define/public (get-inked-extents)
      (cairo_recording_surface_ink_extents s 0. 0. 10. 10.))

    (super-new)))

(define cairo-record-dc% (class (dc-mixin dc-backend%)
                           (super-new)))


;;;
;;; Manual and automatic cropping of picts
;;;

(require (only-in pict draw-pict dc))

(define (crop p width height [x0 0] [y0 0]
                 #:ascent [ascent height]
                 #:descent [descent 0])
  (define (draw-it dc x y) (draw-pict p dc (- x x0) (- y y0)))
  (dc draw-it width height ascent descent))


(define (crop/inked p)
  (define cr-dc (new cairo-record-dc% [x0 0.0] [y0 0.0] [width 1000.] [height 1000.]))
  (draw-pict p cr-dc 0 0)
  (define-values (x y w h) (send cr-dc get-inked-extents))
  (crop p w h x y))

;; (require pict)
;; (crop/inked (disk 20 #:border-width 20))
;; (pict-width (crop/inked (disk 20 #:border-width 20)))
;; (crop/inked (frame (crop/inked (cc-superimpose (blank 100 100) (circle 10)))))
;; (newline)
;; (crop/inked (frame (cc-superimpose (blank 100 100) (circle 10))))
;; (newline)
;; (crop/inked (frame (crop/inked (circle 100))))


;; (require (only-in metapict aligned smoothed unsmoothed))
;; (pict-width (crop/inked (aligned    (disk 20 #:border-width 20))))
;; (pict-width (crop/inked (smoothed   (disk 20 #:border-width 20))))
;; (pict-width (crop/inked (unsmoothed (disk 20 #:border-width 20))))


