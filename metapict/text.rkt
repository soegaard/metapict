#lang racket/base

(require (for-syntax racket/base syntax/parse)
         racket/draw racket/class racket/format
         "pict-lite.rkt" "def.rkt" "color.rkt" "font.rkt" "structs.rkt" "device.rkt" "parameters.rkt")

(provide (rename-out [plain-text text]))

(require (only-in pict text))

(define (plain-text str [font (current-font)] #:text-color [text-color #f])
  (if text-color
      (text str (cons (make-color* text-color) font))
      (text str font)))

