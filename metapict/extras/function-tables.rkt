#lang racket
;;; function-tables.rkt
;; TODO: Generalize the example into a library for function table drawing.

(require metapict
         (only-in srfi/1 zip unzip2)
         (only-in unstable/gui/pict tag-pict find-tag)
         (for-syntax syntax/parse))

(define (tag base i j) (string->symbol (~a base '- i '- j)))

(define (tabular xs ys [tag-base #f])
  (def voffset 4)
  (def hoffset 4)
  (def (pictify x)
    (match x
      [(? pict?)                    x]
      [(or (? number?) (? string?)) (text (~a x))]
      [_ (error 'pictify "can't convert to pict")]))
  (define (adjust-sizes xs ys)
    ; adjust size s.t. all picts get the same height,
    ; and corresponding x and y the same width
    (def h (apply max (map pict-height (append xs ys))))
    (unzip2 (for/list ([xy (zip xs ys)])
              (defm (list x y) xy)
              (def w (max (pict-width x) (pict-width y)))
              (list (cc-superimpose x (blank w h))
                    (cc-superimpose y (blank w h))))))
  (define (add-tags ps row-number)
    (match tag-base
      [#f ps]
      [_  (for/list ([p (in-list ps)] [i (in-naturals)])
            (tag-pict p (tag tag-base row-number i)))]))
  (define (add-vlines xs ys)
    (def h (pict-height (first xs)))
    (define (stack x y)
      (def w (pict-width x))
      (vc-append x (blank w (+ (* 2 voffset) 1)) y))
    (apply hc-append 
           (add-between (map stack xs ys)
                        (hc-append 
                         (blank voffset (+ (* 2 (+ h hoffset)) 1))
                         (vline 1       (+ (* 2 (+ h hoffset)) 1))
                         (blank voffset (+ (* 2 (+ h hoffset)) 1))))))
  (define (add-hline p)
    (cc-superimpose (hline (pict-width p) 1) p))
  (defv (xs* ys*) (adjust-sizes (map pictify xs) (map pictify ys)))
  (add-hline (add-vlines (add-tags xs* 0) (add-tags ys* 1))))

(define-syntax (define-plc-cell stx)
  (syntax-parse stx
    [(_ name finder)
     #'(define (name tabular tagbase row col)
         (devpt (finder tabular (find-tag tabular (tag tagbase row col)))))]))
(define-syntax (define-plc-cell* stx)
  (syntax-parse stx
    [(_ [name finder] ...)
     #'(begin (define-plc-cell name finder) ...)]))
(define-plc-cell* 
  [lt-cell  lt-find]  [ltl-cell ltl-find] [lc-cell  lc-find]  [lbl-cell lbl-find]
  [lb-cell  lb-find]  [ct-cell  ct-find]  [ctl-cell ctl-find] [cc-cell  cc-find]
  [cbl-cell cbl-find] [cb-cell  cb-find]  [rt-cell  rt-find]  [rtl-cell rtl-find]
  [rc-cell  rc-find]  [rbl-cell rbl-find] [rb-cell  rb-find])


(let ()
  (def fig
    (let ()
      (def xs '(  2000   2001  2002   2003 ))
      (def ys (for/list ([y '(  20   22  24   26)]
                         [i (in-naturals)])
                (if (<= i 3) y "")))
      (def t (inset (tabular (cons "x (år)" xs) (cons "y (kr)" ys) 'table) 50))
      (def w (pict-width t))
      (def h (pict-height t))
      (curve-pict-width w)
      (curve-pict-height h)
      (with-window (window 0 w 0 h)
        (ahlength (px 4))
        (define (arrow xx-cell d label label-plc i j k l)
          ; draw arrow from cell (i,j) to (k,l)
          ; xx-cell find the coordinate and d is the angle the arrow leaves (i,j)
          ; and label-plc is used to place the label wrt to the middel of the arrow
          (def A (xx-cell t 'table i j))
          (def B (xx-cell t 'table k l))
          (def AB (curve A (dir d) .. (dir (- d)) B))
          (def AB* (point-of AB 1/2))
          (draw (label-plc label AB*)
                (draw-arrow AB)))
        (scale 4 (inset (draw t 
                              (arrow ct-cell  45 "+1" label-top 0 1 0 2)
                              (arrow ct-cell  45 "+1" label-top 0 2 0 3)
                              (arrow ct-cell  45 "+1" label-top 0 3 0 4)
                              (arrow cb-cell -45 "+2" label-bot 1 1 1 2)
                              (arrow cb-cell -45 "+2" label-bot 1 2 1 3)
                              (arrow cb-cell -45 "+2" label-bot 1 3 1 4))
                        2)))))
  (save-pict "/Users/soegaard/Downloads/fig.png" fig)
  fig)