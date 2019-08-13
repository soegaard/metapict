#lang racket
(require metapict metapict/curve)

(def /- '/-)
(def -/ '-/)
(def --++ '--++)

(define (handle/- p0 p1 more)
  (defm (pt x0 _) p0)
  (defm (pt _ y1) p1)
  (list* p0 -- (pt x0 y1) -- p1 more))

(define (handle-/ p0 p1 more)
  (defm (pt _ y0) p0)
  (defm (pt x1 _) p1)
  (list* p0 -- (pt x1 y0) -- p1 more))

(define (handle--++ p0 v1 more)
  (defm (pt x0 y0) p0)
  (defm (or (pt x1 y1) (vec x1 y1)) v1)
  (list* p0 -- (pt+ p0 (vec x1 y1)) more))

(define (Curve . ds)
  (define simple-path-description
    (let loop ([ds ds])
      (match ds
        [(list* (? pt? p0) '/- (? pt? p1) more)
         (loop (handle/- p0 p1 more))]
        [(list* (? pt? p0) '-/ (? pt? p1) more)
         (loop (handle-/ p0 p1 more))]
        [(list* (? pt? p0) '--++ (or (? pt? p1) (? vec? p1)) more)
         (loop (handle--++ p0 p1 more))]
        [(cons p ps)
         (cons p (loop ps))]
        ['()
         '()])))
  (apply make-curve simple-path-description))


(draw-arrow (Curve (pt 0 0) /- (pt 1/2 1/2) -/ (pt 1 -1)))
(draw-arrow (Curve (pt 0 0) --++ (vec 1/2 1)  --++ (vec 1/2 -1)))

(set-curve-pict-size 400 400)

(define (uml name [fields '()] [methods '()])
  (def amt 4)
  (def n  (inset (font-bold (text name)) 4))
  (def fs (inset (apply vl-append (map text fields))  amt))
  (def ms (inset (apply vl-append (map text methods)) amt))
  (def w  (apply max (map pict-width (list n fs ms))))
  (def l  (hline w 1))
  (above n l (vl-append fs
                        (if (null? methods)
                            (blank w (* 2 amt))
                            (vl-append l ms)))))

;;; Example 1 (From Wikipdia)

(def BankAccount
  (uml "BankAccount"
       '("owner : String"
         "amount : Dollars = 0")
       '("deposit( amount : Dollars )"
         "withdrawal( amount : Dollars )")))

(def BA (rectangle-node BankAccount #:inner-sep 0))
(draw BA)


;;; Example 2

(def Widget   (uml "Widget" '("width : integer" "height : integer")))

(def Label    (uml "Label"))
(def TextArea (uml "TextArea"))
(def Panel    (uml "Panel"))

(def BorderedLabel    (uml "BorderedLabel"))
(def BorderedTextArea (uml "BorderedTextAreal"))
(def BorderedPanel    (uml "BorderedPanel"))

(def ScrollableLabel    (uml "ScrollableLabel"))
(def ScrollableTextArea (uml "ScrollableTextAreal"))
(def ScrollablePanel    (uml "ScrollablePanel"))




(set-curve-pict-size 400 600)
(curve-pict-window (window 0 400 0 600))

(current-inner-separation 0)
(current-neighbour-distance-x (px 60))
(current-neighbour-distance-y (px 20))
(ahlength (px 8))

(def W   (rectangle-node Widget   #:at (pt 200 (- 600 50))))

(def TA  (rectangle-node TextArea #:below    W))
(def L   (rectangle-node Label    #:left-of  TA))
(def P   (rectangle-node Panel    #:right-of TA))

(def BTA (rectangle-node BorderedTextArea #:below TA))
(def BL  (rectangle-node BorderedLabel    #:below L ))
(def BP  (rectangle-node BorderedPanel    #:below P))

(def STA (rectangle-node ScrollableTextArea #:below BTA))
(def SL  (rectangle-node ScrollableLabel    #:below BL))
(def SP  (rectangle-node ScrollablePanel    #:below BP))


(current-edge-color "red")
(current-arrow-color "red")
; (current-arrow-head-color "blue")
; (current-arrow-head-outline-color "black")

(define my-draw-arrow draw-arrow)

(def transparent (change-alpha (make-color* "white") 0))

(draw          W
            L TA P
          BL BTA BP
          STA SL SP
          ; edges from second layer to first
          (my-draw-arrow (Curve (anchor L up) /- (anchor W left))
                         #:head-color "blue")
          (my-draw-arrow (Curve (anchor P up) /- (anchor W right))
                         #:head-outline-color "black"
                         #:head-color "white")
          (edge TA W)
          ; edges from third layer to second layer
          (edge BL L)
          (edge BTA TA)
          (edge BP P)
          ; edges from fourth layer to second layer
          (my-draw-arrow
           (Curve (anchor SL left)  --++ (vec -10 0) /-  (anchor L left)))
          (my-draw-arrow
           (Curve (anchor STA left) --++ (vec -10 0) /-  (anchor TA left)))
          (my-draw-arrow
           (Curve (anchor SP right) --++ (vec 10 0)  /-  (anchor P right))))

