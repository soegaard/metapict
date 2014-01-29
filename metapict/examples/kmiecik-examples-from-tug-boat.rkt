#lang racket

; TODO: fix error from (curve A .. B .. C right .. D .. cycle)

(require "../metapict.rkt")

; http://www.tug.org/TUGboat/tb31-1/tb97kmiecik-logo.pdf
(define (kmiecik-examples)
  (define (u x) (* 1 x))
  (def 1u (u 1))
  (def A (pt  0 0))
  (def B (pt  0 1u))
  (def C (pt 1u 0))
  (def D (pt 1u 1u))
  (with-window (window -1 2 -1 2)
    (map draw (list (curve A          .. B .. C right ..          D)           ; x
                    (curve A left .. right B -- C left .. left D)
                    (curve A -- B -- C -- D -- A)
                    ; (curve A          .. B .. C right ..          D .. cycle)  ; x TODO: This gives an error???
                    ; (curve A (curl 1) .. B .. C right .. (curl 1) D)  
                    ; (curve A (curl 3) .. B .. C right .. (curl 3) D)
                    ; (curve A left .. B .. C right .. left D)             ; x
                    ; (curve A (dir 135) .. B .. D .. C  .. (dir 135) A)
                    ))))

(kmiecik-examples)