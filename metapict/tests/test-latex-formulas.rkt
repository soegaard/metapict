#lang at-exp racket
(require racket-poppler racket-poppler/render-tex
         (only-in pict pict->bitmap hline))

(define current-scale-factor (make-parameter 2))
(define current-preamble (make-parameter ""))

(define (latex x [scale-factor (current-scale-factor)]
               #:preamble [preamble (or (current-preamble) "")])
  (def p (latex->pict x #:preamble preamble #:extract-baseline-fraction? #t))
  (def f (or last-extracted-baseline-fraction 1.1))  
  (defv (w h) (values (pict-width p) (pict-height p)))
  (scale scale-factor p))

(define (math x [scale-factor (current-scale-factor)]
              #:preamble [preamble (or (current-preamble) "")])
  (latex (~a "$" x "$") scale-factor #:preamble preamble))

(define (display-math x [scale-factor (current-scale-factor)]
                      #:preamble [preamble ""])
  (latex (~a "$\\displaystyle " x " $") scale-factor #:preamble preamble))

(current-scale-factor 2)
(math         @~a{a^2+b^2=c^2})
(display-math @~a{\sum_{n=0}^\infty \frac{1}{n^2} = \frac{\pi^2}{6}})


(current-scale-factor 2)

(require metapict)
(set-curve-pict-size 400 400)
(ahlength (px 8))
(current-neighbour-distance (px 40))
(current-inner-separation (px 4))
(current-node-minimum-size (px 32))

(def F (rectangle-node (math "f")))
(def G (rectangle-node (math "g") #:right-of F))
(draw F G
      (edge F G))

(def preamble "\\def\\D{\\mathrm{d}}\n")

(latex-debug? #f)

(curve-pict-window (window -0.5 2.5 -2.5 0.5))
(set-curve-pict-size  400 400)
(current-node-size 0.1)
(current-neighbour-distance 0.8)
(current-label-gap 0.2)
(ahlength 0.1)


(define $ math)

(let ()
  ; A simple commutative diagram
  (def A (text-node ($ "A")))
  (def B (text-node ($ "B") #:right-of A))
  (def C (text-node ($ "C") #:below    B))
  (def D (text-node ($ "D") #:right-of C))
  
  (font-italic
   (margin 5
           (draw #; (color "gray" (grid (pt -3 -3) (pt 3 3)))
            A B C D
            (edge A B #:label ($ "f"))
            (edge A C #:label ($ "g\\circ f"))
            (edge B C #:label ($ "g")         #:arrow '-
                      #:label-dir (vec 0.5 -0.8))
            (edge B D #:label ($ "h\\circ g") #:arrow '<-) 
            (edge C D #:label ($ "h")         #:arrow '<->)))))


(rotate ($ "a^2+b^2=c^3") pi/4)

(latex-debug? #t)

(current-scale-factor 2)
(latex  @~a{\begin{align*}
                  x    &= y    & X   &= Y    & a   & = b+c \\
                  x'   &= y'   & X'  &= Y'   & a'  & = b   \\
                  x+x' &= y+y' & X+X'&= Y+Y' & a'b & = c'b
            \end{align*}})

(def p (math "\\sqrt{xTxy}"))
p

; Resolution:  return dpi * (10.0 / fontsize) * (1000.0 / magnification)
; Preview: Snippet  <number>  <ascent> <descent>
;   frac = ascent / (ascent + descent)
; Preview: Fontsize 10pt
; Preview: Fontsize 10pt
; Preview: Tightpage -32891 -32891 32891 32891
; Preview: Snippet 1 422343 127431 663780
; 282168 0 307200
;(def q 32891)
;(def h 432226) ; ht?
;(def d 127431) ; dp
;(def w 307396) ; wd

; \number\ht
; \pr@box\space
; \number\dp\pr@box \space
; \number\wd\pr@box}}

;as + ... +
;p
;(displayln (list 'h h 'd d 'w w))
;(define pw (pict-width p))
;(define ph (pict-height p))
;(displayln (list 'pw pw 'ph ph))
;(/ w pw)
;(/ h ph)


;Preview: Tightpage -32891 -32891 32891 32891
;Preview: Snippet 1 432226 127431 307396  ; for j

;(- h d)

(define (show-baseline p)
  (defv (w h) (values (pict-width p)  (pict-height p)))
  (defv (a d) (values (pict-ascent p) (pict-descent p)))
  (define line (color "blue" (hline w 0)))
  (pin-over (pin-over (frame p) 0 (- h d) line)
            0 0 p))

(define (show2-baseline p)
  (defv (w h) (values (pict-width p)  (pict-height p)))
  (defv (a d) (values (pict-ascent p) (pict-descent p)))
  (define line (color "red" (hline w 0)))
  (pin-over (pin-over p 0 (- h d) line)
            0 0 p))


(def foo (math "xTT"))
(show-baseline foo)
;(define show show-baseline)
(define show2 show2-baseline)
(define show values)
;(define show2 values)

(require (only-in slideshow para current-font-size current-main-font t))
(current-scale-factor 1)

(current-font-size 10)
(current-main-font ; affects t from slideshow
 (make-similar-font (new-font)
                    #:size 10
                    #:face "CMU Classical Serif"))


(tidy-latex-cache)

(current-preamble
 @~a{
     \usepackage{mathtools}
\usepackage{amssymb}
\usepackage{xcolor}
\newcommand*\drawbaseline[2][orange]
 {\begingroup\sbox0{$\displaystyle#2$}\mathrlap{\color{#1}\rule{\wd0}{.1pt}}\endgroup#2}
})

               (scale 16 (show @math{\drawbaseline{xxx}}))
(show-baseline (scale 16 (show @math{\drawbaseline{xxx}})))


(show2
 (scale 4
        (para #:width 800
              "The"  (show @math{\drawbaseline{xxxy}})
              "xxx"     (show @math{\sqrt{x}})
              "xxx "    (show @math{\frac{a}{b}})
              "The length of the hypotenuse is"              
              " which is easy to prove.")))







(for ([n 80]) (newline))

;;; Small example that show how to TeX formulas can be
;;; rendered into picts. Latest feature is that TeX picts
;;; now has the correct baseline. This means that text
;;; and formulas can be placed next to each other without
;;; problems.

;;; Final version not released yet.

;;; Use the Computer Modern font for normal text.

(current-font-size 10) ; affects para from slideshow
(current-main-font     ; affects para from slideshow
 (make-similar-font (new-font)
                    #:size 10
                    #:face "CMU Classical Serif"))


(scale 4
       (para #:width 200 ; fits my Emacs window
             "A quick example to show that it is possible"
             "to mix text and formulas."
             "First, let's check that the baseline works"
             "by writing the same letters using normal text"
             "and latex:"
             "x"  @math{x}
             "y"  @math{y}
             "xyT" @math{xyT}
             "Now a formula that goes below the baseline"
             @math{\sqrt{x}}
             "and one that must be centered to look good."
             "The fraction" @math{\frac{355}{113}}
             "is close to " @math{\pi} "."))


(for ([n 25]) (newline))
