#lang racket/base
(require "../metapict.rkt"
         racket/draw racket/class)

;;;
;;; Text Paths
;;;

; The file "text-path.rkt" provides a function that turns a text
; into list of curves. These curves can then be manipulated (filled etc).


(define arial (make-similar-font (new-font)
                                 #:size 100
                                 #:face "Roman"
                                 #:weight 'bold
                                 #:style  'slant
                                 ))

(define (window->curve w)
  (defv (p0 p1) (window-opposite-corners w))
  (rectangle p0 p1))


(set-curve-pict-size 400 400)
(scale 1
  (with-window (window -200 200 -200 200)
    (defv (cs bbox) (text-outline arial "Racket" (pt -150 100)
                                  #:return-bounding-box? #t))
    (displayln (list 'bbox bbox))
    (draw
     (grid (pt -200 -200) (pt 200 200) #:step 50)
     ; (grid (pt 0 0) (pt 400 400) #:step 50)
     (window->curve bbox)
     (brushgradient
      (linear-gradient (pt -50 -50) (pt 200 200) '("red" "yellow" "red"))
      (apply fill cs))
     (draw* cs))))

