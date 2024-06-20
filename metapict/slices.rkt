#lang racket/base
(require racket/contract)

(provide slices)        ; Inspired by Mathematica `partition`
(provide extract-every)


; drop-at-most : list nat -> list
;  Like `drop` but returns the empty list, if the list is shorter 
;  than the number of elements to drop. 
(define (drop-at-most xs n)
  (if (or (null? xs) (<= n 0))
      xs
      (drop-at-most (cdr xs) (- n 1))))

; slices : list slice-length [offset] #:keep-short-final? [keep-short-final? #f] -> list
;   Return slices of xs (sublists with consecutive elements of xs) with length n.
;   If `keep-short-final?` is #t, the final slice returned, can have a shorter length.
;   The start of a new slice is found by skipping d elements at a time.
;   If d=n there is no gap and no overlap between slices. This is the default.
;   If d<n there is overlap between the slices.
;   If d>n there is a gap between the slices.
(define/contract (slices xs n [d n] #:keep-short-final? [keep-short-final? #f])
  (->* (list? (and/c natural-number/c (>/c 0)))
       ((and/c natural-number/c (>/c 0)) #:keep-short-final? boolean?)
       list?)
  
  (let loop ([zss '()]   ; list of result slices in reverse order
             [zs  '()]   ; current (incomplete) result slice
             [xs   xs]   ; last starting list
             [ys   xs]   ; current source list
             [i     0])  ; number of processed elements from xs
    ; We repeatedly move elements from ys to zs.
    ; If i=n we must begin on a new slice.
    ; If ys runs "empty", we are ready to return the result.
    (cond
      [(= i n)      (let ([xs (drop-at-most xs d)])
                      (loop (cons zs zss)
                            '()
                            xs
                            xs
                            0))]
      [(null? ys)   (let ([zss (if keep-short-final? (cons zs zss) zss)])
                      (reverse (map reverse zss)))]
      [(< i n)      (loop zss
                          (cons (car ys) zs)
                          xs
                          (cdr ys)
                          (+ i 1))]
      [else         (error)])))

;; (slices '(a b c d e f)   2)                          ; => '((a b) (c d) (e f))
;; (slices '(a b c d e f)   3)                          ; => '((a b c) (d e f))
;; (slices '(a b c d e f g) 3)                          ; => '((a b c) (d e f))
;; (slices '(a b c d e f g) 3 #:keep-short-final? #t)   ; => '((a b c) (d e f) (g))
;; (slices '(a b c d e f g) 3 1)                        ; => '((a b c) (b c d) (c d e) (d e f) (e f g))
;; (slices '(a b c d e f g) 3 2)                        ; => '((a b c) (c d e) (e f g))
;; (slices '(a b c d e f g) 3 3)                        ; => '((a b c) (d e f))
;; (slices '(a b c d e f g) 3 4)                        ; => '((a b c) (e f g))
;; (slices '(a b c d e f g) 3 5)                        ; => '((a b c))
;; (slices '(a b c d e f g) 3 5 #:keep-short-final? #t) ; => '((a b c) (f g))


(define (extract-every xs n)
  ; Return list of every n'th element of `xs`.

  (let loop ([ys '()] [i 0] [xs xs])
    (cond
      [(null? xs) (reverse ys)]
      [(= i 0)    (loop (cons (car xs) ys) (+ i 1) (cdr xs))]
      [(= i n)    (loop                ys     0         xs)]
      [else       (loop                ys  (+ i 1) (cdr xs))])))

;; (extract-every '(a b c d e f g) 1)  ; => '(a b c d e f g)
;; (extract-every '(a b c d e f g) 2)  ; => '(a c e g)
;; (extract-every '(a b c d e f g) 3)  ; => '(a d g)
;; (extract-every '(a b c d e f g) 10) ; => '(a)
;; (extract-every '() 10)              ; => '()
