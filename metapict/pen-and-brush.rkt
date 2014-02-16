#lang racket/base
(provide find-white-transparent-brush
         find-white-transparent-pen)

(require racket/draw racket/class)

(define (find-white-transparent-brush) 
  (send the-brush-list find-or-create-brush "white" 'transparent))

(define (find-white-transparent-pen)
  (send the-pen-list find-or-create-pen "white" 0 'transparent))

(module+ test (require rackunit "def.rkt")
  (def c (send (find-white-transparent-brush) get-color))
  (check-true (and (= (send c red) (send c green) (send c blue) 255)
                   (= (send c alpha) 1.0))))
