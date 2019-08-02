#lang at-exp racket/base
(provide author-jens-axel    ; name and email
         make-metapict-eval  ; make evaluator that knows metapict
         svg-picts           ; style: render picts as svg images
         png-picts           ; style: render picts as png images
         coords              ; (coords a i) -> (a_1,a_2)
         a1 a2 b1 b2         ; a_1 a_2 b_1 b_2
         )

(require scribble/eval scribble/manual scribble/core scribble/base
         scribble/html-properties
         racket/runtime-path)

(define (author-jens-axel)
  @author{@(author+email "Jens Axel Søgaard" "jensaxel@soegaard.net")})

(define-runtime-path here ".")

(define (make-metapict-eval)
  (define eval (make-base-eval))
  (eval '(require racket/format))
  (eval '(require metapict))
  (eval '(require racket/runtime-path))
  (eval `(current-directory ,here))
  eval)

(define svg-picts 
  (make-style "svg-picts" (list (render-convertible-as '(svg-bytes png-bytes)))))

(define png-picts
  
  (make-style "svg-picts" (list (render-convertible-as '(png-bytes svg-bytes)))))

(define (sub a i)
  (nonbreaking a (subscript i)))

(define (coords a)
  (nonbreaking "(" (sub a "1") "," (sub a "2") ")"))

(define a1 (sub "a" "1"))
(define a2 (sub "a" "2"))
(define b1 (sub "b" "1"))
(define b2 (sub "b" "2"))
