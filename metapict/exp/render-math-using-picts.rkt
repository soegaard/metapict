#lang racket
(require racket/draw pict)
(define font-name "Neo Euler")
(send the-font-name-directory find-or-create-font-id font-name 'symbol)

(define font (make-object font% 30 font-name 'symbol))

(define above  vc-append)
(define beside hc-append)
(define (t s) (text s font))

(define (overline p)
  (above (hline (pict-width p) 1)
         p))

(define (vlfit l r)
  ; vertical fit left: scale the left pict to the same height as r
  (beside (scale l (/ (pict-height r) (pict-height l))) 
          r))

(define (super p s)
  (define w (pict-width s))
  (define h (pict-height p))
  (hb-append p (scale (above s (blank w h)) 0.7)))

(define (info p)
    (list (list 'h (pict-height p))
          (list 'a (pict-ascent p))
          (list 'd (pict-descent p))
          (list 'a+d (+ (pict-ascent p) (pict-descent p)))
          (list 'w (pict-width p))))

(info (t "x"))
(info (t "√"))

(send a-dc get-text-extent	 	string	 
 	 [	font	 
 	 	combine?	 
 	 	offset])

(let ([integrand (vlfit (frame (t "√")) (frame (super (t "x") (t "2"))))])
  (beside (above (t "10")
                 (scale (t "∫") 2)
                 (t "1"))
          integrand
          (t " dx")))

