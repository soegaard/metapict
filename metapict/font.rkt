#lang racket/base
;;;
;;; FONT
;;;

(provide make-similar-font
         with-font
         with-font-change

         font-italic font-normal font-slant  ; styles
         font-weight
         font-bold
         font-size
         )


;; Note: The font information can not be stored in the drawing context only.
;;       At the time a text pict is created the font information is needed
;;       in order to compute the location of the base line (ascent and descent).
;;       The drawing context is not accessible, since it pict receives the
;;       drawing context only when it is to be drawn.
;;       The current font is therefore stored in the paramter current-font.

; Note: The names italic and bold are also used scribble, so it is inconvenient
;       to use the same names.

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
         racket/class racket/draw pict syntax/parse/define)
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


;;;
;;; FONT STYLE
;;;
(define-syntax (font-italic stx)
  (syntax-parse stx
    [(_italic e:expr)
     (syntax/loc stx
       (with-font-change (#:style 'italic) e))]))

; name chosen due to clash with normal form node.rkt
(define-syntax (font-normal stx)
  (syntax-parse stx
    [(_font-normal e:expr)
     (syntax/loc stx
       (with-font-change (#:style 'normal) e))]))

(define-syntax (font-slant stx)
  (syntax-parse stx
    [(_slant e:expr)
     (syntax/loc stx
       (with-font-change (#:style 'slant) e))]))


;;;
;;; FONT WEIGHT
;;;

(define-syntax (font-weight stx)
  (syntax-parse stx
    [(_font-weight w:expr e:expr)
     (syntax/loc stx
       (with-font-change (#:weight w) e))]))

(define-simple-macro (font-bold e:expr) (font-weight 'bold e))


;;;
;;; FONT SIZE
;;;

(define-syntax (font-size stx)
  (syntax-parse stx
    [(_font-size n:expr e:expr)
     (syntax/loc stx
       (with-font-change (#:size n) e))]))

