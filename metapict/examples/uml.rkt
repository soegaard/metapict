#lang racket
(require metapict metapict/curve metapict/path-operations)

;;;
;;; UML
;;;


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


;(current-edge-color "red")
;(current-arrow-color "red")
; (current-arrow-head-color "blue")
; (current-arrow-head-outline-color "black")

(define my-draw-arrow draw-arrow)

(def transparent (change-alpha (make-color* "white") 0))

(draw          W
            L TA P
          BL BTA BP
          STA SL SP
          ; edges from second layer to first
          (my-draw-arrow (curve (anchor L up) /- (anchor W left)))
          (my-draw-arrow (curve (anchor P up) /- (anchor W right)))
          (edge TA W)
          ; edges from third layer to second layer
          (edge BL L)
          (edge BTA TA)
          (edge BP P)
          ; edges from fourth layer to second layer
          (my-draw-arrow
           (curve (anchor SL left)  --++ (vec -10 0) /-  (anchor L left)))
          (my-draw-arrow
           (curve (anchor STA left) --++ (vec -10 0) /-  (anchor TA left)))
          (my-draw-arrow
           (curve (anchor SP right) --++ (vec 10 0)  /-  (anchor P right))))


