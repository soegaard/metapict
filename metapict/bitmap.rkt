#lang racket
(provide get-pixel         ; get color% at (x,y) in bitmap
         bitmap-width      
         bitmap-height
         bitmap-size       ; (values width height)
         read-bitmap)      ; read bitmap from file

(require "def.rkt" racket/draw)

; weakly held hash of bitmap dcs.
(define cached-dcs (make-weak-hash))

; get-pixel : bitmap% integer integer -> color%
(define (get-pixel bm x y)
  (define (on-cached-dc-not-found)
    (def bm-dc (new bitmap-dc% [bitmap bm]))
    (hash-set! cached-dcs bm bm-dc)
    bm-dc)
  (def bm-dc (hash-ref cached-dcs bm on-cached-dc-not-found))
  (def col (make-object color%))
  (send bm-dc get-pixel x y col)
  col)

(define (bitmap-width bm)  (send bm get-width))
(define (bitmap-height bm) (send bm get-height))
(define (bitmap-size bm)   (values (bitmap-width bm) (bitmap-height bm)))
