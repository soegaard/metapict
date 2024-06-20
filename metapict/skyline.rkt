#lang racket
(provide skyline
         skyline->lines)

;; Original Source:
;;   https://jeapostrophe.github.io/2013-07-01-skyline-post.html

; The input is a list of building specifications.
; A building specification is a left position, height, and right position. 
; The output is a skyline - a list of points that will be connected by interleaving horizontal and vertical lines.

(require data/heap)
 
(define (skyline input)
  ; <active-set>
  (define A (make-heap (lift > cadr)))
  (heap-add! A (list -inf.0 0 +inf.0))

  ; <visited-set>
  (define V (make-hasheq))
 
  ; <init-D>
  (define D '())
  
  ; <start>
  (define ((start b) O)
    (heap-add! A b)
    (when (> (cadr b) O)
      (set! D (cons (list (car b) (cadr b)) D))))

  ; <end>
  (define ((end b) O)
    (hash-set! V b 1)
    (while (hash-ref V (heap-min A) #f)
      (heap-remove-min! A))
    (define N (cadr (heap-min A)))
    (unless (= O N)
      (set! D (cons (list (caddr b) N) D))))
 
  ; <events>
  (define E (make-heap (lift < car)))
  (for ([b (in-list input)])
    (heap-add! E (cons (car b) (start b)))
    (heap-add! E (cons (caddr b) (end b))))

  ; <run-events>
  (for ([e (in-heap E)])
    ((cdr e) (cadr (heap-min A))))

  ; <return-D>
  (reverse D))
 
(define (lift < f)
  (λ (x y) (< (f x) (f y))))
 
(define-syntax-rule (while C B)
  (let L () (when C B (L))))


;; (require rackunit racket/list)
;; ; <input>
;; (define in
;;   '(( 1 11  5) ( 2  6  7)
;;     ( 3 13  9) (12  7 16)
;;     (14  3 25) (19 18 22)
;;     (23 13 29) (24  4 28)))

;; ; <output>
;; (define out
;;   '(( 1 11) ( 3 13) ( 9  0)
;;     (12  7) (16  3) (19 18)
;;     (22  3) (23 13) (29  0)))

;; ; <basic-test>
;; (check-equal? (skyline in) out)

;; ; <complex-test>
;; (for* ([i (in-range 10)]
;;        [j (in-range 1 10)])
;;   (check-equal?
;;    (skyline (shuffle (append* (build-list j (λ (_) in)))))
;;    out))

(define (skyline->lines key-points)
  (let loop ([cur '(0 0)]       ; current position
             [ls '()]           ; lists lines reprented as lists of (list point point)
             [keys key-points]) ; list of remaining key points
    (cond
      [(null? keys)
       (reverse ls)]
      [else
       (define new (first keys))
       (match-define (list x    y)    new)
       (match-define (list xcur ycur) cur)
       (loop new
             (let ([corner (list x ycur)])
               (list* (list corner new)
                      (list cur corner)
                      ls))
             (cdr keys))])))

;; (require metapict)

;; (define (min* xs) (apply min xs))
;; (define (max* xs) (apply max xs))

;; (define (draw-skyline output)
;;   (define xmin (min* (map first  out)))
;;   (define xmax (max* (map first  out)))
;;   (define ymax (max* (map second out)))
;;   (define xpad 3)
;;   (define ypad 1)

;;   (define lines (skyline->lines key-points))

;;   (with-window (window (- xmin xpad) (+ xmax xpad) (- ypad) (+ ymax ypad))
;;     (draw (for/draw ([line lines])
;;             (match-define (list (list x0 y0) (list x1 y1)) line)
;;             (curve (pt x0 y0) -- (pt x1 y1)))
;;           (curve (pt (- xmin xpad) 0) -- (pt (+ xmax xpad) 0)))))



;; (require "slices.rkt")

;; (define (extract-every xs n)
;;   (append* (slices xs 1 2)))

;; (define (draw-histogram output)
;;   (define xmin (min* (map first  out)))
;;   (define xmax (max* (map first  out)))
;;   (define ymax (max* (map second out)))
;;   (define xpad 3)
;;   (define ypad 1)

;;   (define lines (skyline->lines key-points))

;;   (with-window (window (- xmin xpad) (+ xmax xpad) (- ypad) (+ ymax ypad))
;;     (define roofs (extract-every lines 2))
;;     (define walls (extract-every (rest lines) 2))
;;     (draw
;;      (for/draw ([line roofs])
;;        (match-define (list (list x0 y0) (list x1 y1)) line)
;;        (curve (pt x0 y0) -- (pt x1 y1)))
;;      (for/draw ([line walls])
;;        (match-define (list (list x0 y0) (list x1 y1)) line)
;;        (curve (pt x0 (max y0 y1)) -- (pt x0 0)))
;;      (curve (pt (- xmin xpad) 0) -- (pt (+ xmax xpad) 0)))))

;; (draw-skyline out)

;; (draw-histogram out)


