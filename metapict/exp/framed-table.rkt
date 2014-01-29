#lang slideshow
(require (only-in srfi/1 zip unzip1 unzip2))
(define x-title "Flagstangens højde (m)")
(define y-title "Flagets højde (m)")
(define xs '(8.0  12.0))
(define ys '( 1.70  2.45))

(define number->pict   (compose t ~a))
(define numbers->picts (curry map number->pict))

(define (same-widths picts #:skip [skip 0] #:justification [justification 'center])
  (define-values (as ps) (split-at picts skip))
  (define w (apply max (map pict-width ps)))
  (define (adjust p) 
    (match justification
      ['center (inset p (/ (- w (pict-width p)) 2) 0)]
      ['left   (inset p 0 0 (- w (pict-width p)) 0)]
      ['right  (inset p (- w (pict-width p)) 0 0 0)]))
  (append as (map adjust ps)))

(define (same-heights picts #:skip [skip 0])
  (define-values (as ps) (split-at picts skip))
  (define h (apply max (map pict-height ps)))
  (define (adjust p) (inset p 0 (/ (- h (pict-height p)) 2)))
  (append as (map adjust ps)))

(define (frame-table x-title y-title xs ys)
  (define-values (xs* ys*) (unzip2 (map same-widths (zip xs ys))))
  (match-define (list xt yt) (same-widths (list x-title y-title) #:justification 'center))
  (define (frame* p) (frame (inset p (pict-width (t " ")))))
  (vl-append  (apply hc-append (map frame* (cons xt xs*)))
              (apply hc-append (map frame* (cons yt ys*)))))

(define flagstangstabel
  (frame-table (t x-title) (t y-title)
               (numbers->picts xs) (numbers->picts ys)))

(slide (t "Du har købt en flagstang, men mangler et flag.")
       (t "Hvor stort skal det være?"))

(slide (t "Eksempler på flag og flagstænger")
       (t "TODO: Tegninger med flag og flagstænger")
       (t "Det pæneste?"))

(slide (t flagstangstabel)
       'next
       "Tabellen viser højder på stang og flag som ser pæne ud sammen."
       "Din flagstang er 9.2m høj.")
