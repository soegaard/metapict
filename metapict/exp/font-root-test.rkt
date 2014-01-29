#lang racket
(require racket/draw pict)

(define (info p)
    (list (list 'h (pict-height p))
          (list 'a (pict-ascent p))
          (list 'd (pict-descent p))
          (list 'a+d (+ (pict-ascent p) (pict-descent p)))
          (list 'w (pict-width p))))

(define s "x")

(define (map-fonts f)
  (for/list ([font-name '("Latin Modern Math" "Neo Euler" "Default" "Arial" 
                                              "Courier" "Palatino" "Cambria")])
    (define font (make-object font% 30 font-name 'symbol))
    (f font)))


(map-fonts (λ(font) (inset (frame (text s font)) 10)))
"Info from pict box"
(map-fonts (λ(font) (info (text s font))))
"Info from get-text-extent"
(map-fonts (λ(font) 
             (define-values (x y z v) (send (dc-for-text-size) get-text-extent s font))
             (list x y z v)))

