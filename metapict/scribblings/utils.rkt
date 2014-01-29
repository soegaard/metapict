#lang at-exp racket/base
(require scribble/eval
         scribble/manual
         scribble/core 
         scribble/html-properties)

(provide author-jens-axel
         make-metapict-eval
         svg-picts)

(define (author-jens-axel)
  @author{@(author+email "Jens Axel Søgaard" "jensaxel@soegaard.net")})

(define (make-metapict-eval)
  (define eval (make-base-eval))
  (eval '(require racket/format))
  (eval '(require metapict))
  eval)

(define svg-picts 
  (make-style "svg-picts" (list (render-pict-as 'svg-images))))