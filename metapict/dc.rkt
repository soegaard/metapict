#lang racket
;;; This module contains functions related to drawing contexts.

(require racket/draw "def.rkt" "structs.rkt" "trans.rkt")

(define dc? any/c) ; todo easy: make contract

(provide make-bitmap-dc
         (contract-out 
          [set-transformation (-> dc? trans? void)] ; set the transformation of the drawing context
          ))

(define (set-transformation dc t)
  (send dc set-transformation 
        (vector (trans->vector t)
                0  0 ; x and y origin
                1  1 ; x and y scale
                0))) ; rotation

(define (make-bitmap-dc width height)
  (def bm (make-object bitmap% width height))
  (def dc (new bitmap-dc% [bitmap bm]))
  (send dc set-smoothing 'smoothed)  ; <- important otherwise bezier curves are off
  (send dc set-background "white")
  (send dc set-brush "white" 'transparent)
  (send dc clear)
  (send dc set-pen "black" 1 'solid)
  dc)