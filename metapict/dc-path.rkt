#lang racket
(provide dc-path->bitmap)

(require racket/draw "def.rkt" "dc.rkt")

(define (dc-path->bitmap p width height)
  (def dc (make-bitmap-dc width height))
  (send dc draw-path p)
  (send dc get-bitmap))