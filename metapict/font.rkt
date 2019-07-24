#lang racket/base
;;;
;;; FONT
;;;

(provide make-similar-font
         with-font
         with-font-change)


;; Note: The font information can not be stored in the drawing context only.
;;       At the time a text pict is created the font information is needed
;;       in order to compute the location of the base line (ascent and descent).
;;       The drawing context is not accessible, since it pict receives the
;;       drawing context only when it is to be drawn.
;;       The current font is therefore stored in the paramter current-font.

;; The font contains:

; size
; face
; family
; style
; weight
; underlined
; smoothing
; hinting 

(require "parameters.rkt" "def.rkt"
         racket/class racket/draw pict)
(require (for-syntax racket/base racket/syntax syntax/parse))

; make-similar-font font keyword-arguments -> font
;   create or find a new font similar to font,
;   but using with some properties changes.
(define (make-similar-font font
                           #:size            [new-size            #f]
                           #:face            [new-face            #f]
                           #:family          [new-family          #f]
                           #:style           [new-style           #f]
                           #:weight          [new-weight          #f]
                           #:underlined?     [new-underlined      #f]
                           #:smoothing       [new-smoothing       #f]
                           #:size-in-pixels? [new-size-in-pixels  #f]
                           #:hinting         [new-hinting         #f])
  (def f font)
  (def size            (send f get-size))
  (def face            (send f get-face))
  (def family          (send f get-family))
  (def style           (send f get-style))
  (def weight          (send f get-weight))
  (def underlined      (send f get-underlined))
  (def smoothing       (send f get-smoothing))
  (def size-in-pixels  (send f get-size-in-pixels))
  (def hinting         (send f get-hinting))
  (make-font #:size            (or new-size size)
             #:face            (or new-face face)
             #:family          (or new-family family)
             #:style           (or new-style style)
             #:weight          (or new-weight weight)
             #:underlined?     (or new-underlined underlined)
             #:smoothing       (or new-smoothing smoothing)
             #:size-in-pixels? (or new-size-in-pixels size-in-pixels)
             #:hinting         (or new-hinting hinting)))

(define-syntax (with-font-change stx)
  (syntax-parse stx
    [(_with-similar-font (keywords ...) . body)
     (with-syntax ([make (format-id #'make-similar-font "make-similar-font" #:source stx)])
       (syntax/loc stx
         (parameterize
             ([current-font (make (current-font) keywords ...)])
           . body)))]))

(define-syntax (with-font stx)
  (syntax-parse stx
    [(_with-font font . body)
     (syntax/loc stx
       (parameterize ([current-font font])
         . body))]))
