#lang racket
(provide find-white-transparent-brush)

(require racket/draw)

(define (find-white-transparent-brush) 
  (send the-brush-list find-or-create-brush "white" 'transparent))

(module+ test (require rackunit "def.rkt")
  (def c (send (find-white-transparent-brush) get-color))
  (check-true (and (= (send c red) (send c green) (send c blue) 255)
                   (= (send c alpha) 1.0))))
